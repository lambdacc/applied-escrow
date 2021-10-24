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


mainTest :: IO ()
mainTest = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [publishAndAcceptTest]

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
testContractAmount = 5_000_000

testTrancheCount :: Integer
testTrancheCount = 4

publishAndAcceptTest :: TestTree
publishAndAcceptTest = checkPredicate "Publish and accept test"
    ( walletFundsChange w1 mempty
      .&&. walletFundsChange w2 (Ada.lovelaceValueOf $ negate testContractAmount)
    )
    $ runTrace1

runTrace1 ::EmulatorTrace ()
runTrace1 = do
    h1 <- activateContractWallet w1 startEscrowEndpoint
    h2 <- activateContractWallet w2 acceptEscrowEndpoint

    let pkh1      = (pubKeyHash . walletPubKey) $ w1
        pkh2      = (pubKeyHash . walletPubKey) $ w2
        amount    = testContractAmount
        deadline1 = slotToBeginPOSIXTime def 10
        deadline2 = slotToBeginPOSIXTime def 15

        param = PublishParam
                { p      = pkh1
                , c      = pkh2
                , st     = deadline1
                , et     = deadline2
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
                    , ust     = deadline1
                    , uet     = deadline2
                    , ull     = amount
                    , utc     = testTrancheCount
                    , uttn    = tt
                    }

    callEndpoint @"accept" h2 useParam
    void $ Emulator.waitNSlots 1


--Lifecycle stage 1,2 tests
publishAcceptAndPayTranchesTest :: TestTree
publishAcceptAndPayTranchesTest = checkPredicate "Publish, accept and pay tranches tests"
    ( walletFundsChange w1 (Ada.lovelaceValueOf $ testContractAmount)
      .&&. walletFundsChange w2 (Ada.lovelaceValueOf $ negate testContractAmount)
    )
    $ runTrace2

runTrace2 ::EmulatorTrace ()
runTrace2 = do
    h1 <- activateContractWallet w1 startEscrowEndpoint
    h2 <- activateContractWallet w2 acceptEscrowEndpoint

    let pkh1      = (pubKeyHash . walletPubKey) $ w1
        pkh2      = (pubKeyHash . walletPubKey) $ w2
        amount    = testContractAmount
        deadline1 = slotToBeginPOSIXTime def 10
        deadline2 = slotToBeginPOSIXTime def 15

        param = PublishParam
                { p      = pkh1
                , c      = pkh2
                , st     = deadline1
                , et     = deadline2
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
              , ust     = deadline1
              , uet     = deadline2
              , ull     = amount
              , utc     = testTrancheCount
              , uttn    = tt
              }

    callEndpoint @"accept" h2 useParam
    void $ Emulator.waitNSlots 1