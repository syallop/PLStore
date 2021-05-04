{-# LANGUAGE OverloadedStrings #-}
module PLStore.Test.DHTSpec where

import Test.Hspec

-- Module under test
import PLStore.DHT
import PLStore

-- Other PL
import PLPrinter.Doc

import Data.ByteString (ByteString)
import Data.Text
import Data.Text.Encoding
import Control.Concurrent

type TestKey = Text

type TestValue = Text

type TestDHTStore = DHTStore TestKey TestValue

serialize :: Text -> ByteString
serialize   = encodeUtf8

deserialize :: ByteString -> Either Doc Text
deserialize = either (\unicodeErr
                       -> Left . mconcat $
                               [ text "Failed to deserialize text with unicode error:"
                               , lineBreak
                               , string . show $ unicodeErr
                               ]
                     )
                     Right
            . decodeUtf8'

newTestDHT :: IO TestDHTStore
newTestDHT = newDHTStore serialize serialize deserialize (Just (putStrLn . ("Log: " <>)))

testLookup :: TestDHTStore -> TestKey -> IO (Either Doc (Maybe TestValue))
testLookup dht key = lookupInDHT dht key

testStore :: TestDHTStore -> TestKey -> TestValue -> IO (Either Doc (StoreResult TestValue))
testStore dht key value = storeInDHT dht key value

spec :: Spec
spec = do
  describe "DHT" $ do
      describe "can be created" $ do
        dht <- runIO $ newTestDHT
        it "does not return non-existant keys" $ do
          emValue <- testLookup dht "key000"
          emValue `shouldBe` Right Nothing

        it "can have keys stored" $ do
          eStoreResult <- testStore dht "key00001" "value"
          eStoreResult `shouldBe` Right Successfully

        it "reads back the expected value" $ do
          threadDelay 1000000
          emValue <- testLookup dht "key00001"
          emValue `shouldBe` Right (Just "value")

