{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-specialize #-}

-- | Simplified SM-based contract for the purpose of developing the interface
-- between Node and Chain
module Hydra.ContractSM where

import Control.Lens (makeClassyPrisms)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Hydra.Prelude (Eq, Show, String, show, void)
import Ledger (AssetClass, PubKeyHash)
import Ledger.Ada (lovelaceValueOf)
import Ledger.Constraints (mustPayToPubKey)
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.Contract (
  AsContractError (..),
  BlockchainActions,
  Contract,
  ContractError (..),
  Endpoint,
  currentSlot,
  endpoint,
  logInfo,
  mapError,
  throwError,
  type (.\/),
 )
import Plutus.Contract.StateMachine (StateMachine, StateMachineClient, WaitingResult (..))
import qualified Plutus.Contract.StateMachine as SM
import qualified Plutus.Contracts.Currency as Currency
import qualified PlutusTx
import PlutusTx.Prelude hiding (Eq)

data State
  = Setup
  | Initial
  | Open
  | Final
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''State
PlutusTx.unstableMakeIsData ''State

data Input
  = Init [PubKeyHash]
  | CollectCom
  | Abort
  deriving (Generic, Show)

PlutusTx.makeLift ''Input
PlutusTx.unstableMakeIsData ''Input

data HydraPlutusError
  = -- | State machine operation failed
    SMError SM.SMContractError
  | -- | Endpoint, coin selection, etc. failed
    PlutusError ContractError
  | -- | Thread token could not be created
    ThreadTokenError Currency.CurrencyError
  | -- | Arbitrary error
    HydraError String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''HydraPlutusError

instance AsContractError HydraPlutusError where
  _ContractError = _PlutusError

instance SM.AsSMContractError HydraPlutusError where
  _SMContractError = _SMError

{-# INLINEABLE hydraStateMachine #-}
hydraStateMachine :: AssetClass -> StateMachine State Input
hydraStateMachine _threadToken =
  -- XXX(SN): This should actually be '(Just threadToken)' as we wan't to have
  -- "contract continuity" as described in the EUTXO paper. Unfortunately we did
  -- not get this yet to work with 'runStep', or at least we were expecting the
  -- statemachine library would handle it for us.
  SM.mkStateMachine Nothing hydraTransition isFinal
 where
  isFinal Final{} = True
  isFinal _ = False

{-# INLINEABLE hydraTransition #-}
hydraTransition :: SM.State State -> Input -> Maybe (SM.TxConstraints SM.Void SM.Void, SM.State State)
hydraTransition oldState input =
  case (SM.stateData oldState, input) of
    (Setup, Init pubKeyHashes) ->
      Just (mempty, oldState{SM.stateData = Initial})
     where
      _constraints = foldMap (\pubKey -> mustPayToPubKey pubKey (lovelaceValueOf 1)) pubKeyHashes
    _ -> Nothing

-- | The script instance of the auction state machine. It contains the state
-- machine compiled to a Plutus core validator script. The 'AssetClass' serves
-- two roles here:
--
--   1. Parameterizing the script, such that we get a unique address and allow
--   for multiple instances of it
--
--   2. Identify the 'state thread token', which should be passed in
--   transactions transitioning the state machine and provide "contract
--   continuity"
typedValidator :: AssetClass -> Scripts.TypedValidator (StateMachine State Input)
typedValidator currency =
  let val =
        $$(PlutusTx.compile [||validatorParam||])
          `PlutusTx.applyCode` PlutusTx.liftCode currency
      validatorParam c = SM.mkValidator (hydraStateMachine c)
      wrap = Scripts.wrapValidator @State @Input
   in Scripts.mkTypedValidator @(StateMachine State Input)
        val
        $$(PlutusTx.compile [||wrap||])

-- | The machine client of the hydra state machine. It contains both, the script
-- instance with the on-chain code, and the Haskell definition of the state
-- machine for off-chain use.
machineClient ::
  -- | Thread token of the instance
  AssetClass ->
  StateMachineClient State Input
machineClient threadToken =
  let machine = hydraStateMachine threadToken
      inst = typedValidator threadToken
   in SM.mkStateMachineClient (SM.StateMachineInstance machine inst)

setup :: Contract () (BlockchainActions .\/ Endpoint "init" [PubKeyHash]) HydraPlutusError AssetClass
setup = do
  logInfo @String "setup hydra contract"
  threadToken <- mapError ThreadTokenError Currency.createThreadToken
  logInfo $ "Obtained thread token: " <> show @String threadToken

  let client = machineClient threadToken
  void $ SM.runInitialise client Setup mempty

  logInfo @String "SM initialised, waiting for pubkeys"
  -- NOTE: These are the cardano/chain keys to send PTs to
  pubkeyHashes <- endpoint @"init" @[PubKeyHash]

  logInfo $ "Got pubkeys :" <> show @String pubkeyHashes

  void $ SM.runStep client (Init pubkeyHashes)
  logInfo $ "Triggering Init " <> show @String pubkeyHashes

  pure threadToken

-- | Wait for 'Init' transaction to appear on chain and return the observed state of the state machine
watchInit :: AssetClass -> Contract () BlockchainActions HydraPlutusError State
watchInit threadToken = do
  logInfo @String $ "watchInit: Looking for an init tx for SM: " <> show threadToken
  let client = machineClient threadToken
  sl <- currentSlot
  SM.waitForUpdateUntil client (sl + 10) >>= \case
    (Timeout _s) -> throwError $ HydraError "Timed out waiting for transaction"
    ContractEnded -> throwError $ HydraError "Contract ended"
    (WaitingResult s) -> pure s
