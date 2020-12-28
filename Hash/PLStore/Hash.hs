{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , RankNTypes
           , GADTs
           , LambdaCase
           , ConstraintKinds
           , OverloadedStrings
  #-}
{-|
Module      : PLStore.Hash
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

A HashStore is a data structure that stores and retrieves values by their Hash.

It is constructed with the more general Store for backing storage and handles:
- Generating keys as Hashes of values on storage operations
- Verifying the contents of values on lookup
-}
module PLStore.Hash
  ( HashStore ()
  , HashBackingStorage
  , newHashStore

  , HashStoreResult (..)
  , storeByHash
  , lookupByHash

  , ShortHash ()
  , mkBase58ShortHash
  , unBase58ShortHash
  , largerHashes
  , shortenHash
  )
  where

import Prelude hiding (lookup)

-- PLStore
import PLStore
import PLStore.Short

-- External PL
import PLHash
import PLHash.Short
import PLPrinter

-- Other
import Data.Map (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text
import Data.Text.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as B58
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text

-- | Store values 'v' by their 'Hash' in some underlying 'Store'.
data HashStore v
  = forall s
  . ( ShortStore s Hash ShortHash v
    , Hashable v
    , Show (s Hash v)
    )
 => HashStore
      { _store :: s Hash v
      }

instance Show (HashStore v) where
  show h = case h of
    HashStore s
      -> show s

type HashBackingStorage s v = ShortStore s Hash ShortHash v

-- | Create a HashStore from some underlying backing Storage which accepts
-- 'Hash'es as keys.
newHashStore
  :: ( Hashable v
     , HashBackingStorage s v
     , Show (s Hash v)
     )
  => s Hash v
  -> HashStore v
newHashStore s = HashStore s

-- | The result of successfully storing something against it's Hash.
data HashStoreResult k = HashStoreResult
  { _storedAgainstHash :: Hash          -- ^ The hash key used
  , _storeResult       :: StoreResult k -- ^ The type of success
  }

-- | Store a value 'v' in the 'HashStore' using the provided algorithm to
-- generate the Hash that will be used as the key.
--
-- Returns a potentially updated HashStore, a description of the success and the
-- key that can be used to retrieve the original value.
--
-- E.G.
-- h :: HashStore Text
-- h = newHashStore . newNestedStore newEmptyMemoryStore . newFileStore ".store" $ Just 32
--
-- Constructs a HashStore that:
-- - Caches in-memory
-- - Stores values in files under `.store`, using a 32 character prefix for subdirectories
storeByHash
  :: HashStore v
  -> HashAlgorithm
  -> v
  -> IO (Either Doc (HashStore v, HashStoreResult v))
storeByHash h alg v = case h of
  HashStore s
    -> do let hashKey = hashWith alg v
          mResult <- store s hashKey v
          case mResult of
            Left err
              -> pure . Left $ err

            Right (s', res)
              -> pure . Right $ (HashStore s', HashStoreResult hashKey res)

-- | Retrieve a store value from a HashStore using it's Hash as a key.
lookupByHash
  :: HashStore v
  -> Hash
  -> IO (Either Doc (HashStore v, Maybe v))
lookupByHash h hashKey = case h of
  HashStore s
    -> lookup s hashKey >>= \case
         Left err
           -> pure . Left $ err

         Right (s', mValue)
           -> case mValue of
                Nothing
                  -> pure . Right $ (HashStore s', Nothing)

                Just value
                  -> let valueHash = hashWith (hashAlgorithm hashKey) value
                      in if valueHash /= hashKey
                           then error $ mconcat [ "HashStore returned an incorrect value for a hash. Asked for hash \n"
                                                , show hashKey
                                                , "\n but returned value hashed to \n"
                                                , show valueHash
                                                ]
                           else pure . Right $ (HashStore s', Just value)

-- | Given a ShortHash, return all known larger Hashes
largerHashes
  :: HashStore v
  -> ShortHash
  -> IO (Either Doc (HashStore v, [Hash]))
largerHashes (HashStore s) shortHash = fmap (\(s',hashes) -> (HashStore s',hashes)) <$> largerKeys s shortHash

-- | Given a regular Hash, return the shortest unambiguous Hash.
shortenHash
  :: HashStore v
  -> Hash
  -> IO (Either Doc (HashStore v, ShortHash))
shortenHash (HashStore s) hash = fmap (\(s',shortHash) -> (HashStore s',shortHash)) <$> shortenKey s hash

