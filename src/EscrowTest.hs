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
testContractAmount = 8_000_000

testTrancheCount :: Integer
testTrancheCount = 4

testContractStartTime :: POSIXTime
testContractStartTime = slotToBeginPOSIXTime def 10

testContractEndTime :: POSIXTime
testContractEndTime = slotToBeginPOSIXTime def 18

runTrace1 ::EmulatorTrace ()
runTrace1 = do
    h1 <- activateContractWallet w1 startEscrowEndpoint
    h2 <- activateContractWallet w2 useEscrowEndpoint

    let pkh1      = (pubKeyHash . walletPubKey) $ w1
        pkh2      = (pubKeyHash . walletPubKey) $ w2
        amount    = testContractAmount
        startTime = testContractStartTime
        endTime   = testContractEndTime

        param = PublishParam
                { p      = pkh1
                , c      = pkh2
                , st     = startTime
                , et     = endTime
                , tc     = testTrancheCount
                , ll     = amount
                }

    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" h1 (param, True)
    tt <- getTT h1
    void $ Emulator.waitNSlots 1

    let useParam = UseParam
                    { up      = pkh1
                    , uc      = pkh2
                    , ust     = startTime
                    , uet     = endTime
                    , ull     = amount
                    , utc     = testTrancheCount
                    , uttn    = tt
                    }

    callEndpoint @"accept" h2 useParam

    void $ Emulator.waitNSlots 1

-- Provider shouldn't be able to collect funds before  start time
runTrace2 ::EmulatorTrace ()
runTrace2 = do
    h1 <- activateContractWallet w1 startEscrowEndpoint
    h2 <- activateContractWallet w2 useEscrowEndpoint
    h3 <- activateContractWallet w1 collectEscrowEndpoint

    let pkh1      = (pubKeyHash . walletPubKey) $ w1
        pkh2      = (pubKeyHash . walletPubKey) $ w2
        amount    = testContractAmount
        startTime = testContractStartTime
        endTime   = testContractEndTime

        param = PublishParam
                { p      = pkh1
                , c      = pkh2
                , st     = startTime
                , et     = endTime
                , tc     = testTrancheCount
                , ll     = amount
                }

    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" h1 (param, True)
    tt <- getTT h1
    void $ Emulator.waitNSlots 1

    let useParam = UseParam
                    { up      = pkh1
                    , uc      = pkh2
                    , ust     = startTime
                    , uet     = endTime
                    , ull     = amount
                    , utc     = testTrancheCount
                    , uttn    = tt
                    }

    callEndpoint @"accept" h2 useParam
    void $ Emulator.waitNSlots 2
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 1

-- Provider collects first tranche but cannot collect second tranche right away
runTrace3 ::EmulatorTrace ()
runTrace3 = do
    h1 <- activateContractWallet w1 startEscrowEndpoint
    h2 <- activateContractWallet w2 useEscrowEndpoint
    h3 <- activateContractWallet w1 collectEscrowEndpoint

    let pkh1      = (pubKeyHash . walletPubKey) $ w1
        pkh2      = (pubKeyHash . walletPubKey) $ w2
        amount    = testContractAmount
        startTime = testContractStartTime
        endTime   = testContractEndTime

        param = PublishParam
                { p      = pkh1
                , c      = pkh2
                , st     = startTime
                , et     = endTime
                , tc     = testTrancheCount
                , ll     = amount
                }

    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" h1 (param, True)
    tt <- getTT h1
    void $ Emulator.waitNSlots 1

    let useParam = UseParam
                    { up      = pkh1
                    , uc      = pkh2
                    , ust     = startTime
                    , uet     = endTime
                    , ull     = amount
                    , utc     = testTrancheCount
                    , uttn    = tt
                    }

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
    h2 <- activateContractWallet w2 useEscrowEndpoint
    h3 <- activateContractWallet w1 collectEscrowEndpoint

    let pkh1      = (pubKeyHash . walletPubKey) $ w1
        pkh2      = (pubKeyHash . walletPubKey) $ w2
        amount    = testContractAmount
        startTime = testContractStartTime
        endTime   = testContractEndTime

        param = PublishParam
                { p      = pkh1
                , c      = pkh2
                , st     = startTime
                , et     = endTime
                , tc     = testTrancheCount
                , ll     = amount
                }

    void $ Emulator.waitNSlots 1
    callEndpoint @"publish" h1 (param, True)
    tt <- getTT h1
    void $ Emulator.waitNSlots 1

    let useParam = UseParam
                    { up      = pkh1
                    , uc      = pkh2
                    , ust     = startTime
                    , uet     = endTime
                    , ull     = amount
                    , utc     = testTrancheCount
                    , uttn    = tt
                    }

    callEndpoint @"accept" h2 useParam
    void $ Emulator.waitUntilSlot 10
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 1
    callEndpoint @"collect" h3 useParam
    void $ Emulator.waitNSlots 1
