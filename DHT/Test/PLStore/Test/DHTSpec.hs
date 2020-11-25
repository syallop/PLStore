{-# LANGUAGE OverloadedStrings #-}
module PLStore.Test.DHTSpec where

import Test.Hspec

-- Module under test
import PLStore.DHT
import PLStore

-- Other PL
import PL.Error
import PL.Expr
import PL.FixPhase
import PL.TypeCtx

import Data.Text
import Control.Concurrent

type TestKey = Text

type TestValue = Text

type TestDHTStore = DHTStore TestKey TestValue

newTestDHT :: IO TestDHTStore
newTestDHT = newDHTStore

testLookup :: TestDHTStore -> TestKey -> IO (Either Error (Maybe TestValue))
testLookup dht key = lookupInDHT dht key

testStore :: TestDHTStore -> TestKey -> TestValue -> IO (Either Error (StoreResult TestValue))
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

