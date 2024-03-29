{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RankNTypes
           , LambdaCase
           , TupleSections
           , OverloadedStrings
  #-}
{-|
Module      : PLStore.Nested
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

Nested implementation of PLStore which caches values in 'cheaper' stores which
can then evict values safely.

-}
module PLStore.Nested
  ( NestedStore ()
  , newNestedStore
  , lookupNested
  , storeNested

  , largerNestedKeys
  , shortenNestedKeys
  )
  where

import Prelude hiding (lookup)

-- PLStore
import PLStore
import PLStore.Short

-- Other PL
import PLPrinter.Doc

-- External
import qualified Data.List as List

-- | A Nested store behaves as if the top level store is an inexpensive cache
-- for some more expensive (but more reliable) lower level store.
--
-- E.G. Top-Level In-Memory Store and a nested File-System backed store.
--
-- A Nested Store should maintain the property that a value stored in a higher
-- store is already stored in the nested store.
--
-- The top level store _may_ evict items as necessary. The nested store should
-- not.
--
-- Nested stores can themselves be nested.
data NestedStore s s' k v = NestedStore
  { _topStore    :: s  k v -- ^ A Top level store should be less expensive than the nested store and may evict items if necessary.
  , _nestedStore :: s' k v -- ^ A nested store may be more expensive than the top store and should not evict items.
  }
  deriving Show

instance
  (Store s  k v
  ,Store s' k v
  ,Ord v
  ,Show k
  ,Show v
  )
  => Store (NestedStore s s') k v where
  store  = storeNested
  lookup = lookupNested

instance
  (Store s  k v
  ,Store s' k v
  ,Ord v
  ,Show k
  ,Show v
  ,Show shortK
  ,Shortable k shortK
  ,ShortStore s k shortK v
  ,ShortStore s' k shortK v
  ) => ShortStore (NestedStore s s') k shortK v where
  largerKeys = largerNestedKeys
  shortenKey = shortenNestedKeys

-- | Create a new nested store from two stores.
--
-- The first top-level store should be 'cheaper' to access and will be used to
-- cache results. It may evict data.
--
-- The second nested store can be more expensive but should not evict data.
--
-- The second nested store can itself be a nested store.
newNestedStore
  :: store k v
  -> store' k v
  -> NestedStore store store' k v
newNestedStore s0 s1 = NestedStore s0 s1

-- | Lookup a value associated with a key in a nested store.
--
-- If a result is found in the top store, the nested store will not be
-- consulted.
--
-- If a result is only found in the nested store, it will be cached at the top
-- for cheaper subsequent access.
lookupNested
  :: ( Store s  k v
     , Store s' k v
     )
  => NestedStore s s' k v
  -> k
  -> IO (Either Doc (NestedStore s s' k v, Maybe v))
lookupNested (NestedStore topStore nestedStore) key =
  lookup topStore key >>= \case
    Left err
      -> pure . Left $ err

    -- In the top store. We can assume the value is also contained in the nested
    -- store.
    Right (topStore', Just value)
      -> pure . Right $ (NestedStore topStore' nestedStore, Just value)

    -- Not in top store, check nested store.
    Right (topStore',Nothing)
      -> lookup nestedStore key >>= \case
           Left err
             -> pure . Left $ err

           -- Not in either store.
           Right (nestedStore', Nothing)
             -> pure . Right $ (NestedStore topStore' nestedStore', Nothing)


           -- In the nested store. Cache in the top store and return.
           Right (nestedStore', Just value)
             -> store topStore key value >>= \case
                  Left err
                    -> pure . Left $ err

                  -- TODO: Can we not ignore the result?
                  Right (topStore'', _res)
                    -> pure . Right $ (NestedStore topStore'' nestedStore', Just value)

-- | Store a key-value in the nested store by ensuring:
-- - It is first stored in the nested store.
-- - It is then cached in the top-level store.
--
-- Failure at any level will cause the overall store to return Nothing.
-- Stores should be safe to retry until success in the case of transient
-- failures.
--
-- It is currently an exception for the nested store to be unaware of a key that
-- the top-level store has cached. This is because it indicates either:
-- - The nested store has performed eviction/ is less reliable than promised.
-- - The co-ordination between stores is broken.
--
-- When the library is more stable this could become a recoverable error/ values
-- could be silently added to the nested store. For now, you should ensure
-- nested stores do not evict requested data.
storeNested
  :: ( Store s  k v
     , Store s' k v
     , Ord v
     , Show k
     , Show v
     )
  => NestedStore s s' k v
  -> k
  -> v
  -> IO (Either Doc (NestedStore s s' k v, StoreResult v))
storeNested (NestedStore topStore nestedStore) key value = lookup topStore key >>= \case
  Left err
    -> pure . Left $ err

  -- TODO: Merge the many similar branches.
  -- TODO: Consider reporting partial success when we store in nested stores
  -- but encounter failures caching at the top.
  -- TODO: Consider being more permissive about data-loss/ errors in nested
  -- stores when they are unaware of values we know of at the top level.
  -- Currently this is an exception but it could be recovered/ reported more
  -- nicely.

  -- Not in the top store, proceed to check the nested store.
  Right (topStore', Nothing)
    -> lookup nestedStore key >>= \case
         Left err
           -> pure . Left $ err

         Right (nestedStore', mRes)
           -> case mRes of

                -- Value isn't in the nested store, proceed to store and cache.
                Nothing
                  -> storeNestedThenTop topStore' nestedStore key value

                -- Not in the top store but is in the nested store.
                Just nestedValue

                  -- The value in the nested store is what we're trying to store.
                  -- Cache in the top store and we're done.
                  | nestedValue == value
                   -> fmap (fmap (\(top,res)
                                   -> (NestedStore top nestedStore', res <> AlreadyStored)
                                 )
                           )
                           $ store topStore' key value

                  -- Nested store has a different value, replace it, cache the
                  -- replacement in the higher store and return the old values
                  | otherwise
                   -> storeNestedThenTop topStore' nestedStore' key value


  -- In the top store. If identical, we're done. Otherwise, recurse.
  Right (topStore', Just topValue)
    | topValue == value
     -> pure . Right $ (NestedStore topStore' nestedStore, AlreadyStored)

    -- Different value in the top store, recurse and replace on the way back
    -- up returning the old value
    | otherwise
     -> lookup nestedStore key >>= \case
          Left err
            -> pure . Left $ err

          -- Value isn't stored in nested and is different in the top.
          -- If everything is behaving correctly this shouldnt happen. It
          -- implies either:
          --
          -- - A store algorithm (incorrectly) added something to a higher
          --  store before storing in the nested store and we're witnessing a
          --  race condition.
          --
          -- - A value has been evicted from the nested store but not the
          --   top cache. Stores should be layered such that nested store
          --   does not evict data.
          --
          -- While developing, we're going to fail loudly here.
          --
          -- TODO: Consider tolerating this invarient being broken and
          -- attempting to store in the nested stores anyway.
          Right (_,Nothing)
            -> error $ mconcat
                 [ "When storing a key-value in a nested store we:\n"
                 , "- Found a different value at the top store (which we assume to be outdated)\n"
                 , "- Found no value at the nested store\n"
                 , "This could imply data-loss at the nested store which _should_ not evict values.\n"
                 , "Since we're in development mode we're failing loudly here, instead of attempting to clean up. Sorry!\n"
                 , "Context:\n"
                 , "Key:", show key, "\n"
                 , "New value:", show value, "\n"
                 , "Top value:", show topValue, "\n"
                 ]


          -- Value is in the nested store. If identical we only need to cache
          -- in the top store. If different, both need to be replaced.
          Right (nestedStore', Just nestedValue)
            -- Value is already stored here. Cache in the top store.
            | nestedValue == value
             -> fmap (fmap (\(topStore'', topStoreResult)
                             -> (NestedStore topStore'' nestedStore', topStoreResult <> AlreadyStored)
                           )
                     )
                     $ store topStore' key value

            -- Value is different in both the cache and the nested store.
            -- Update it.
            | otherwise
             -> storeNestedThenTop topStore' nestedStore key value


-- Attempt to store in the second nested store. Then only if successful the
-- first top-level store.
storeNestedThenTop
  :: ( Store s k v
     , Store s' k v
     , Ord v
     )
  => s k v
  -> s' k v
  -> k
  -> v
  -> IO (Either Doc (NestedStore s s' k v, StoreResult v))
storeNestedThenTop top nested key value =
  store nested key value >>= \case
    Left err
      -> pure . Left $ err

    Right (nested', nestedResult)
     -> do store top key value >>= \case
             Left err
               -> pure . Left $ err

             Right (top', topResult)
               -> pure . Right $ (NestedStore top' nested', topResult <> nestedResult)

-- | Lookup all known keys that are 'larger' than a short key.
largerNestedKeys
  :: (ShortStore s' k shortK v)
  => NestedStore s s' k v
  -> shortK
  -> IO (Either Doc (NestedStore s s' k v, [k]))
largerNestedKeys (NestedStore top nested) shortKey = largerKeys nested shortKey >>= \case
  -- TODO: If the interface was a stream/ returned batches of results and a continuation|done then
  -- We could query the top store first and potentially avoid touching the
  -- nested store if the consumer could find what it wanted early.
  --
  -- As we have to return all results, we must query the nested store so we
  -- might as well skip the top store.

  Left err
    -> pure . Left $ err

  Right (nestedStore', larger)
    -> pure . Right $ (NestedStore top nestedStore', larger)

-- | Shorten a key to the smallest possible unambiguous ShortKey.
shortenNestedKeys
  :: ( ShortStore s  k shortK v
     , ShortStore s' k shortK v
     , Show k
     , Show shortK
     )
  => NestedStore s s' k v
  -> k
  -> IO (Either Doc (NestedStore s s' k v, shortK))
shortenNestedKeys (NestedStore top nested) key = shortenKey top key >>= \case
  -- Algorithm:
  -- - Lookup shortest prefix in top
  -- - Query that prefix in the nested store
  --   - If one result: done
  --   - If multiple results: Pick the shortest
  Left err
    -> pure . Left $ err

  Right (top', candidateShortKey)
   -> largerKeys nested candidateShortKey >>= \case
        Left err
          -> pure . Left $ err

        -- Nested store doesn't know of any larger keys.
        Right (_, [])
          -> pure . Left . mconcat $
                        [ text "In a nested Store the top-level shortened a key to a value which has no larger keys in the nested store."
                        , lineBreak
                        , text "This implies bad data in the top store or that the nested store has suffered data loss."
                        , lineBreak
                        , text "Key:\t", string . show $ key
                        , lineBreak
                        , text "Candidate short key:\t", string . show $ candidateShortKey
                        , lineBreak
                        ]

        -- Nested store agrees this is the shortest key
        -- TODO: Could sanity check the returned key is actually a
        -- larger key.
        Right (nestedStore', [_shortKey])
          -> pure . Right $ (NestedStore top' nestedStore', candidateShortKey)

        -- Nested store knows of more than one colliding key
        -- Compute against these.
        Right (nestedStore', shortKeys)
          -> pure . Right . (NestedStore top' nestedStore',) . Prelude.head . List.sortOn shortLength . fmap (shortenAgainst key . Just) $ shortKeys

