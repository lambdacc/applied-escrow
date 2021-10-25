{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module EscrowTest where

import           AppliedEscrow
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Ledger
import           Ledger.TimeSlot
import           Ledger.Value
import           Ledger.Ada                 as Ada
import           Numeric.Natural
import           Plutus.Contract.Test       ((.&&.), walletFundsChange, checkPredicate)
import           Plutus.Contract.Trace      as X
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude
import           Prelude                    (IO, Show (..), String)
import           Test.Tasty
import           Wallet.Emulator.Wallet


test1 :: IO ()
test1 = runEmulatorTraceIO $ runTrace1

test2 :: IO ()
test2 = runEmulatorTraceIO $ runTrace2

test3 :: IO ()
test3 = runEmulatorTraceIO $ runTrace3

test4 :: IO ()
test4 = runEmulatorTraceIO $ runTrace4

test5 :: IO ()
test5 = runEmulatorTraceIO $ runTrace5

test6 :: IO ()
test6 = runEmulatorTraceIO $ runTrace6

w1, w2, w3 :: Wallet
w1 = X.knownWallet 1
w2 = X.knownWallet 2
w3 = X.knownWallet 3


getTT :: ContractHandle (Last ThreadToken) StartAppliedEscrowSchema Text -> EmulatorTrace ThreadToken
getTT h = do
    void $ Emulator.waitNSlots 1
    Last m <- observableState h
    case m of
        Nothing -> getTT h
        Just tt -> Extras.logInfo ("read thread token " ++ show tt) >> return tt

testContractAmount :: Integer
testContractAmount = 80_000_000

testTrancheCount :: Integer
testTrancheCount = 4

testContractStartTime :: POSIXTime
testContractStartTime = slotToBeginPOSIXTime def 10

testContractEndTime :: POSIXTime
testContractEndTime = slotToBeginPOSIXTime def 18

buildPublishParam :: PublishParam
buildPublishParam =
  PublishParam
    { p      = pkh1
    , c      = pkh2
    , st     = startTime
    , et     = endTime
    , tc     = testTrancheCount
    , ll     = amount
    }
    where
      pkh1      = (pubKeyHash . walletPubKey) $ w1
      pkh2      = (pubKeyHash . walletPubKey) $ w2
      amount    = testContractAmount
      startTime = testContractStartTime
      endTime   = testContractEndTime

buildUseParam :: ThreadToken -> UseParam
buildUseParam tt =
  UseParam
    { up      = pkh1
    , uc      = pkh2
    , ust     = startTime
    , uet     = endTime
    , ull     = amount
    , utc     = testTrancheCount
    , uttn    = tt
    }
    where
      pkh1      = (pubKeyHash . walletPubKey) $ w1
      pkh2      = (pubKeyHash . walletPubKey) $ w2
      amount    = testContractAmount
      startTime = testContractStartTime
      endTime   = testContractEndTime

runTrace1 ::EmulatorTrace ()
runTrace1 = do
    h1 <- activateContractWallet w1 startEscrowEndpoint
    h2 <- activateContractWallet w2 useEscrowEndpoints

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" h1 param
    tt <- getTT h1

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" h2 useParam

    void $ Emulator.waitNSlots 1

-- Provider shouldn't be able to collect funds before  start time
runTrace2 ::EmulatorTrace ()
runTrace2 = do
    h1 <- activateContractWallet w1 startEscrowEndpoint
    h2 <- activateContractWallet w2 useEscrowEndpoints
    h3 <- activateContractWallet w1 collectEscrowEndpoint

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" h1 param
    tt <- getTT h1

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" h2 useParam

    void $ Emulator.waitNSlots 2
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 1

-- Provider collects first tranche but cannot collect second tranche right away
runTrace3 ::EmulatorTrace ()
runTrace3 = do
    h1 <- activateContractWallet w1 startEscrowEndpoint
    h2 <- activateContractWallet w2 useEscrowEndpoints
    h3 <- activateContractWallet w1 collectEscrowEndpoint

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" h1 param
    tt <- getTT h1

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" h2 useParam

    void $ Emulator.waitUntilSlot 10
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 1

-- Provider collects all tranches at the eligible intervals
runTrace4 ::EmulatorTrace ()
runTrace4 = do
    h1 <- activateContractWallet w1 startEscrowEndpoint
    h2 <- activateContractWallet w2 useEscrowEndpoints
    h3 <- activateContractWallet w1 collectEscrowEndpoint

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" h1 param
    tt <- getTT h1

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" h2 useParam

    void $ Emulator.waitUntilSlot 10
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 2
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 2
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 2
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 2

-- Once consumer raises dispute (after tranche2) the provider cannot collect funds, consumer cannot close within three days
runTrace5 ::EmulatorTrace ()
runTrace5 = do
    h1 <- activateContractWallet w1 startEscrowEndpoint
    h2 <- activateContractWallet w2 useEscrowEndpoints
    h3 <- activateContractWallet w1 collectEscrowEndpoint

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" h1 param
    tt <- getTT h1

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" h2 useParam

    void $ Emulator.waitUntilSlot 10
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 2
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 2
    callEndpoint @"dispute" h2 useParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitUntilSlot 117
    callEndpoint @"close" h2 useParam

-- Disputed after first tranche, consumer can claim back funds after max settlement time - 3 days after end time, has elapsed
runTrace6 ::EmulatorTrace ()
runTrace6 = do
    h1 <- activateContractWallet w1 startEscrowEndpoint
    h2 <- activateContractWallet w2 useEscrowEndpoints
    h3 <- activateContractWallet w1 collectEscrowEndpoint

    let param = buildPublishParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" h1 param
    tt <- getTT h1

    void $ Emulator.waitNSlots 1
    let useParam = buildUseParam tt
    callEndpoint @"accept" h2 useParam

    void $ Emulator.waitUntilSlot 10
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 2
    callEndpoint @"dispute" h2 useParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitUntilSlot 118
    callEndpoint @"close" h2 useParam
