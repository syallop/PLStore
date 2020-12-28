{-# LANGUAGE MultiParamTypeClasses
  , FunctionalDependencies
  , OverloadedStrings
  #-}
module PLStore.Short
  (
    Shortable ()
  , shortenAgainst
  , shortLength
  , isShortened
  , toShort

  , ShortStore ()
  , largerKeys
  , shortenKey
  )
  where

-- PLStore
import PLStore

-- External PL
import PLPrinter
import PLHash.Short

-- External
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8,decodeUtf8)
import qualified Data.ByteString as BS

-- A ShortStore is a Store which understands how to deal with shortened keys.
class (Store s k v, Shortable k shortK) => ShortStore s k shortK v where
  -- | Given a ShortKey, return all known larger keys.
  largerKeys
    :: s k v
    -> shortK
    -> IO (Either Doc (s k v, [k]))

  -- | Given a regualar key, return the shortest unambiguous key.
  shortenKey
    :: s k v
    -> k
    -> IO (Either Doc (s k v, shortK))

