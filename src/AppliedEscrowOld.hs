{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module AppliedEscrow
    ( AppliedEscrow (..)
    , EscrowRedeemer (..)
    , PublishParam (..)
    , UseParam (..)
    , StartAppliedEscrowSchema
    , UseAppliedEscrowSchema
    , Last (..)
    , ThreadToken
    , Text
    , startEscrowEndpoint
    , acceptEscrowEndpoint
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

data AppliedEscrow = AppliedEscrow
    { provider        :: !PubKeyHash
    , consumer        :: !PubKeyHash
    , startTime       :: !POSIXTime
    , endTime         :: !POSIXTime
    , contractValue   :: !Value
    , eTT             :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''AppliedEscrow

data EscrowRedeemer =
          Accept
        | Withdraw
        | Dispute
        | Close
        deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

{-Is there a better way to do makeIsDataIndexed ?-}
PlutusTx.makeLift ''EscrowRedeemer
PlutusTx.makeIsDataIndexed ''EscrowRedeemer [('Accept, 0), ('Withdraw, 0), ('Dispute, 0), ('Close, 0)]

data EscrowDatum = Published Integer | Active Integer | Withdrawn | Disputed | Closed
    deriving Show

PlutusTx.makeIsDataIndexed ''EscrowDatum [('Published, 1), ('Active, 1), ('Withdrawn, 1), ('Disputed, 1), ('Closed, 1)]


{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE transition #-}
transition :: AppliedEscrow -> State EscrowDatum -> EscrowRedeemer -> Maybe (TxConstraints Void Void, State EscrowDatum)
transition escrow s r = case (escrow, stateValue s, stateData s, r) of
    (e, v, d, Accept)               -> Just ( Constraints.mustBeSignedBy (consumer escrow) <>
                                               Constraints.mustValidateIn (to $ startTime escrow)
                                             , State (Active $ lovelaces $ contractValue e) $
                                               v <>
                                               contractValue e
                                             )
    _                               -> Nothing

{-# INLINABLE isClosed #-}
isClosed :: EscrowDatum -> Bool
isClosed Closed = True
isClosed _ = True

{-# INLINABLE escrowStateMachine #-}
escrowStateMachine :: AppliedEscrow -> StateMachine EscrowDatum EscrowRedeemer
escrowStateMachine e = mkStateMachine (Just $ eTT e) (transition e) (isClosed)

{-# INLINABLE mkEscrowValidator #-}
mkEscrowValidator :: AppliedEscrow -> EscrowDatum -> EscrowRedeemer -> ScriptContext -> Bool
mkEscrowValidator escrow = mkValidator $ escrowStateMachine escrow

type EscrowSMType = StateMachine EscrowDatum EscrowRedeemer

escrowTypedValidator :: AppliedEscrow -> Scripts.TypedValidator EscrowSMType
escrowTypedValidator escrow = Scripts.mkTypedValidator @EscrowSMType
    ($$(PlutusTx.compile [|| mkEscrowValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode escrow)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @EscrowDatum @EscrowRedeemer

escrowValidator :: AppliedEscrow -> Validator
escrowValidator = Scripts.validatorScript . escrowTypedValidator

escrowAddress :: AppliedEscrow -> Ledger.Address
escrowAddress = scriptAddress . escrowValidator

escrowClient :: AppliedEscrow -> StateMachineClient EscrowDatum EscrowRedeemer
escrowClient e = mkStateMachineClient $ StateMachineInstance (escrowStateMachine e) (escrowTypedValidator e)

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

data PublishParam = PublishParam
    { p   :: !PubKeyHash
    , c   :: !PubKeyHash
    , st  :: !POSIXTime
    , et  :: !POSIXTime
    , ll  :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

publish :: forall s. PublishParam -> Bool -> Contract (Last ThreadToken) s Text ()
publish param useTT = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  tt  <- mapError' getThreadToken
  logInfo @String $ "Logging thread token: " ++ show tt
  let escrow   = AppliedEscrow
          { provider        = pkh
          , consumer        = c param
          , startTime       = st param
          , endTime         = et param
          , contractValue   = lovelaceValueOf $ ll param
          , eTT             = tt
          }
      client = escrowClient escrow
  void $ mapError' $ runInitialise client (Published $ ll param) mempty
  tell $ Last $ Just tt
  logInfo @String $ "Escrow contract published: " ++ show escrow


data UseParam = UseParam
    { up   :: !PubKeyHash
    , uc   :: !PubKeyHash
    , ust  :: !POSIXTime
    , uet  :: !POSIXTime
    , ull  :: !Integer
    , uttn :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

accept :: UseParam -> Contract w s Text ()
accept param = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  let escrow   = AppliedEscrow
              { provider        = up param
              , consumer        = pkh
              , startTime       = ust param
              , endTime         = uet param
              , contractValue   = lovelaceValueOf $ ull param
              , eTT             = uttn param
              }

      client = escrowClient escrow
  st <- mapError' $ getOnChainState client
  case st of
    Nothing   ->            throwError "Nothing found for on chain state"
    Just (OnChainState{ocsTxOut=TypedScriptTxOut{tyTxOutData=Published a}}, _) ->
                    do
                      logInfo @String "Published escrow contract found"
                      void $ mapError' $ runStep client $ Accept
                      logInfo @String $ "Contract accepted: " ++ show escrow
    Just a    ->
                    do
                      logInfo @String "Something found"
                      void $ mapError' $ runStep client $ Accept
                      logInfo @String $ "Contract accepted: " ++ show escrow

    {-Just ((o, _), _) -> case tyTxOutData o of
            Published a -> do
                logInfo @String "Published escrow contract found"
                void $ mapError' $ runStep client $ Accept
                logInfo @String $ "Contract accepted: " ++ show escrow
-}

type StartAppliedEscrowSchema = Endpoint "publish" (PublishParam, Bool)

type UseAppliedEscrowSchema = Endpoint "accept" UseParam

{-endpoints :: Contract (Last ThreadToken) AppliedEscrowSchema Text ()
endpoints = (publish) >> endpoints
  where
    publish  = endpoint @"publish"  >>= publish-}

startEscrowEndpoint :: Contract (Last ThreadToken) StartAppliedEscrowSchema Text ()
startEscrowEndpoint = forever
                        $ handleError logError
                        $ awaitPromise
                        $ endpoint @"publish" (\(x,y) -> publish x y)

acceptEscrowEndpoint :: Contract (Last ThreadToken) UseAppliedEscrowSchema Text ()
acceptEscrowEndpoint = forever
                        $ handleError logError
                        $ awaitPromise
                        $ endpoint @"accept" (\x -> accept x)

