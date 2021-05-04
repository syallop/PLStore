{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , RankNTypes
           , FlexibleContexts
           , OverloadedStrings
           , GADTs
           , TupleSections
           , ScopedTypeVariables
  #-}
{-|
Module      : PLStore.DHT
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

PLStore backed by a Distributed HashTable.

-}
module PLStore.DHT
  ( DHTStore ()
  , newDHTStore
  , shutdownDHT
  , storeInDHT
  , lookupInDHT
  , logInDHT
  )
  where

-- PLStore
import PLStore
import PLStore.Short

import DHT
import DHT.Core
import DHT.SimpleNode

import PLPrinter.Doc

-- External
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString      as Strict
import qualified Data.ByteString.Lazy.Char8 as Lazy
import Data.Text (Text)
import qualified Data.Text as Text

{- The implementation assumes that all store/ lookup commands must be made
   within the same DHT computation to produce a sane result and therefore:
   - Writes requests to an event channel
   - Reads events from the event channel within a single computation

   TODO: While this might be a reasonable way to abstract the implementation,
   the current concrete implementation should not require it - I.E. as long as
   it's config is retained, each function can run a new DHT computation.
   Consider whether this internal abstraction is worthwhile or not.
-}

-- | Store key-values in an Distributed HashTable.
data DHTStore k v = DHTStore
  { _eventChan           :: Chan (DHTStoreRequest k v)

  , _serializeDHTKey     :: k -> Strict.ByteString

  , _serializeDHTValue   :: v -> Strict.ByteString
  , _deserializeDHTValue :: Strict.ByteString -> Either Doc v

  , _hashSize            :: Int
  }

instance Show (DHTStore k v) where
  show _ = "DHTStore"

-- | Create a new store backed by a Distributed HashTable that will handle
-- commands until it is asked to close.
newDHTStore
  :: forall k v
   . Show k
  => (k -> Strict.ByteString)
  -> (v -> Strict.ByteString)
  -> (Strict.ByteString -> Either Doc v)
  -> Maybe (String -> IO ())
  -> IO (DHTStore k v)
newDHTStore serializeKey serializeValue deserializeValue logging = do
  -- Hardcode some configuration
  let ourIPAddress      = fromParts (IPV4 "127.0.0.1") [UDP 6470]
      sharedHashSize    = 256
      mBootstrapAddress = Nothing

  -- TODO: The 'SimpleNode' implementation is a loosely tested example
  -- implementation, we should replace.
  simpleNodeConfig
    <- mkSimpleNodeConfig ourIPAddress sharedHashSize logging mBootstrapAddress

  -- Channel on which DHTStoreRequest's will be sent
  eventChan
    <- newChan

  let dhtStore = DHTStore
        { _eventChan       = eventChan

        , _serializeDHTKey     = serializeKey
        , _serializeDHTValue   = serializeValue
        , _deserializeDHTValue = deserializeValue

        , _hashSize = sharedHashSize
        }

  -- Handle dht operations in a new thread until a shutdown command is read from
  -- the eventChan.
  _ <- forkIO . void . newSimpleNode simpleNodeConfig . handleDHTCommands $ dhtStore

  pure dhtStore

-- We inject the original store into the result since it acts as a handle which
-- doesnt 'update' itself.
instance Store DHTStore k v where
  store  s k v = fmap (s,) <$> storeInDHT s k v
  lookup s k   = fmap (s,) <$> lookupInDHT s k

-- DHT's cannot currently (efficiently) shorten/ largen keys.
-- For now, these will be a no-op.
--
-- The impact is:
-- 1. Long keys will never be shortened.
-- 2. Any key which is not present in another store, cannot be resolved by short key.
--    This is _okay_ as long as:
--    - Memory/ file based stores are used in conjunction.
--    - These stores don't have eviction functionality implemented.
--    - Users retain long keys as the canonical form.
--    - Short keys are sourced from recent user storage
-- If any of those properties become untrue, short-keys will degrade to long keys.
--
-- For the PL user, these properties currently hold. The main drawback is that
-- externally discovered short-hashes cannot be resolved.
--
-- TODO: Add commands for shortening/ lengthening to the DHT/ change the
-- behaviour/ guarantees of nesting stores.
instance Shortable key shortKey => ShortStore DHTStore key shortKey v where
  largerKeys s _shortKey = pure . Right . (s,) $ []
  shortenKey s key       = pure . Right . (s,) . toShort $ key

-- A request to the dht store is a command that should return some response
-- value 'r' by writing it to the provided response mvar.
data DHTStoreRequest k v = forall r. DHTStoreRequest
  { _responseMVar   :: MVar (Either Doc r) -- ^ Space in which to write the response that can be waited upon.
  , _requestCommand :: DHTStoreCommand k v r
  }

-- Model the commands that can be sent to the underlying DHT server.
-- r is the type of result expected from a command.
data DHTStoreCommand k v r where

  -- | Request the DHT to shutdown and stop serving commands.
  ShutdownDHT
    :: DHTStoreCommand k v ()

  -- | Lookup a key, returning the possible value.
  LookupDHT
    :: k
    -> DHTStoreCommand k v (Maybe v)

  -- | Store an association between a key and a value in the DHT.
  StoreDHT
    :: k
    -> v
    -> DHTStoreCommand k v (StoreResult v)

  -- | Log a message using the DHTs configured logging
  LogDHT
    :: Text
    -> DHTStoreCommand k v ()

-- handle commands from the underlying channel one by one.
handleDHTCommands
  :: Show k
  => DHTStore k v
  -> DHT IO ()
handleDHTCommands dhtStore = do
  DHTStoreRequest response cmd <- liftDHT . readChan . _eventChan $ dhtStore
  case cmd of
    ShutdownDHT
      -> do lg "shutting down DHT"
            -- TODO:
            -- - Consider attempting to process remaining commands in the
            --   queue.
            -- - Consider properly releasing the nodes tcp port.
            liftDHT $ putMVar response (Right ())

    LookupDHT key
      -> do emValue <- handleLookup key dhtStore
            case emValue of
              Left err
                -> lg $ "Error from handleLookup: " <> (Text.unpack . render . document $ err)

              Right _mValue
                -> lg "Success from handleLookup"

            liftDHT $ putMVar response emValue
            handleDHTCommands dhtStore

    StoreDHT key value
      -> do lg "handleDHTCommands: store request received"
            eStoreResult <- handleStore key value dhtStore
            lg "handleDHTCommands: store result received, putting in response mvar"
            liftDHT $ putMVar response eStoreResult
            handleDHTCommands dhtStore

    LogDHT msg
      -> do lg (Text.unpack msg)
            handleDHTCommands dhtStore

-- | Shutdown the DHT, running any relevant cleanup actions.
shutdownDHT
  :: DHTStore k v
  -> IO (Either Doc ())
shutdownDHT dhtStore = do
  response <- newEmptyMVar
  writeChan (_eventChan dhtStore) $ DHTStoreRequest response $ ShutdownDHT
  takeMVar response

-- | Retrieve a key-value association from a Distributed HashTable.
lookupInDHT
  :: DHTStore k v
  -> k
  -> IO (Either Doc (Maybe v))
lookupInDHT dhtStore key = do
  response <- newEmptyMVar
  writeChan (_eventChan dhtStore) $ DHTStoreRequest response $ LookupDHT key
  takeMVar response

-- | Store a key-value association in the Distributed HashTable.
storeInDHT
  :: DHTStore k v
  -> k
  -> v
  -> IO (Either Doc (StoreResult v))
storeInDHT dhtStore key value = do
  response <- newEmptyMVar
  writeChan (_eventChan dhtStore) $ DHTStoreRequest response $ StoreDHT key value
  takeMVar response

-- | Log a message within the DHT logging context.
logInDHT
  :: DHTStore k v
  -> Text
  -> IO (Either Doc ())
logInDHT dhtStore msg = do
  response <- newEmptyMVar
  writeChan (_eventChan dhtStore) $ DHTStoreRequest response $ LogDHT msg
  takeMVar response

-- translate a lookup command into an operation on the underlying dht.
handleLookup
  :: Show k
  => k
  -> DHTStore k v
  -> DHT IO (Either Doc (Maybe v))
handleLookup key dhtStore = do
  let keyID = generateKeyID dhtStore key
  -- TODO: Report contacts
  (_contacts, mValue) <- findValue keyID
  lg $ mconcat
    ["lookup:\n"
    ,"  input key:     ", show key,"\n"
    ,"  lookup key ID: ", show keyID, "\n"
    ]
  case mValue of
    Nothing
      -> pure $ Right Nothing

    Just value
      -> pure . fmap Just . (_deserializeDHTValue dhtStore) . Lazy.toStrict $ value

-- translate a store command into an operation on the underlying dht.
handleStore
  :: (Show k)
  => k
  -> v
  -> DHTStore k v
  -> DHT IO (Either Doc (StoreResult v))
handleStore key value dhtStore  = do
  let serializedKey   = Lazy.fromStrict . (_serializeDHTKey   dhtStore) $ key
  let serializedValue = Lazy.fromStrict . (_serializeDHTValue dhtStore) $ value
  returnedKeyID <- DHT.store serializedKey serializedValue
  lg $ mconcat
    ["stored:\n"
    ,"  input key:       ", show key, "\n"
    ,"  serialized key:  ", show serializedKey, "\n"
    ,"  returned key id: ", show returnedKeyID, "\n"
    ]
  return . Right $ Successfully

-- Generate the ID a key will be given by similarly configured DHT's.
-- This currently depends on the hash size.
generateKeyID
  :: DHTStore k v
  -> k
  -> ID
generateKeyID dhtStore key =
  let serializedKey = Lazy.fromStrict . (_serializeDHTKey dhtStore) $ key
   in mkID serializedKey (_hashSize dhtStore)

