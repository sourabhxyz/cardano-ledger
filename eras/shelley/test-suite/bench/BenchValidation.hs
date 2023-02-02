{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-binds #-}

module BenchValidation (
  ValidateInput (..),
  validateInput,
  benchValidate,
  benchreValidate,
  applyBlock,
  sizes,
  updateChain,
  updateAndTickChain,
  genUpdateInputs,
) where

import Cardano.Ledger.BaseTypes (Globals (..), unBlocksMade)
import Cardano.Ledger.Block (Block (..))
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CryptoClass
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Bench.Gen (genBlock, genChainState)
import Cardano.Ledger.Shelley.BlockChain (slotToNonce)
import Cardano.Ledger.Shelley.LedgerState (
  NewEpochState,
  PPUPState,
  ShelleyPPUPState,
  StashedAVVMAddresses,
  nesBcur,
 )
import Cardano.Protocol.TPraos.API (
  ChainDepState (..),
  ChainTransitionError,
  GetLedgerView,
  LedgerView (..),
  currentLedgerView,
  tickChainDepState,
  updateChainDepState,
 )
import Cardano.Protocol.TPraos.BHeader (BHeader (..), LastAppliedBlock (..), makeHeaderView)
import Cardano.Protocol.TPraos.Rules.Prtcl (PrtclState (..))
import Cardano.Protocol.TPraos.Rules.Tickn (TicknState (..))
import Cardano.Slotting.Slot (withOriginToMaybe)
import Control.DeepSeq (NFData (rnf))
import Control.Monad.Except ()
import qualified Control.State.Transition.Trace.Generator.QuickCheck as QC
import Data.Proxy
import Test.Cardano.Ledger.Shelley.ConcreteCryptoTypes (Mock)
import Test.Cardano.Ledger.Shelley.Generator.Core (GenEnv)

-- Use Another constraint, so this works in all Eras

import Cardano.Ledger.Shelley.Core (EraTallyState (..))
import qualified Data.Map.Strict as Map
import Test.Cardano.Ledger.Shelley.Generator.EraGen (EraGen, MinLEDGER_STS)
import Test.Cardano.Ledger.Shelley.Generator.Presets (genEnv)
import Test.Cardano.Ledger.Shelley.Rules.Chain (ChainState (..))
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.Shelley.Utils (testGlobals)

data ValidateInput era = ValidateInput Globals (NewEpochState era) (Block (BHeader (EraCrypto era)) era)

sizes :: ValidateInput era -> String
sizes (ValidateInput _gs ss _blk) = "blockMap size=" ++ show (Map.size (unBlocksMade (nesBcur ss)))

instance NFData (ValidateInput era) where
  rnf (ValidateInput a b c) = seq a (seq b (seq c ()))

validateInput ::
  ( EraGen era
  , Mock (EraCrypto era)
  , EraRule "LEDGERS" era ~ API.ShelleyLEDGERS era
  , QC.HasTrace (API.ShelleyLEDGERS era) (GenEnv era)
  , API.ApplyBlock era
  , GetLedgerView era
  , PPUPState era ~ ShelleyPPUPState era
  , MinLEDGER_STS era
  , EraTallyState era
  ) =>
  Int ->
  IO (ValidateInput era)
validateInput utxoSize = genValidateInput utxoSize

genValidateInput ::
  ( EraGen era
  , Mock (EraCrypto era)
  , EraRule "LEDGERS" era ~ API.ShelleyLEDGERS era
  , QC.HasTrace (API.ShelleyLEDGERS era) (GenEnv era)
  , API.ApplyBlock era
  , GetLedgerView era
  , MinLEDGER_STS era
  , PPUPState era ~ ShelleyPPUPState era
  , EraTallyState era
  ) =>
  Int ->
  IO (ValidateInput era)
genValidateInput n = do
  let ge = genEnv (Proxy :: Proxy era)
  chainstate <- genChainState n ge
  block <- genBlock ge chainstate
  pure (ValidateInput testGlobals (chainNes chainstate) block)

benchValidate ::
  forall era.
  ( Era era
  , API.ApplyBlock era
  ) =>
  ValidateInput era ->
  IO (NewEpochState era)
benchValidate (ValidateInput globals state (Block bh txs)) =
  case API.applyBlock @era globals state (UnsafeUnserialisedBlock (makeHeaderView bh) txs) of
    Right x -> pure x
    Left x -> error (show x)

applyBlock ::
  forall era.
  ( EraTxOut era
  , API.ApplyBlock era
  , NFData (StashedAVVMAddresses era)
  , PPUPState era ~ ShelleyPPUPState era
  , NFData (TallyState era)
  ) =>
  ValidateInput era ->
  Int ->
  Int
applyBlock (ValidateInput globals state (Block bh txs)) n =
  case API.applyBlock @era globals state (UnsafeUnserialisedBlock (makeHeaderView bh) txs) of
    Right x -> seq (rnf x) (n + 1)
    Left x -> error (show x)

benchreValidate ::
  ( Era era
  , API.ApplyBlock era
  ) =>
  ValidateInput era ->
  NewEpochState era
benchreValidate (ValidateInput globals state (Block bh txs)) =
  API.reapplyBlock globals state (UnsafeUnserialisedBlock (makeHeaderView bh) txs)

-- ==============================================================

data UpdateInputs c
  = UpdateInputs
      !Globals
      !(LedgerView c)
      !(BHeader c)
      !(ChainDepState c)

instance CryptoClass.Crypto c => Show (UpdateInputs c) where
  show (UpdateInputs _globals vl bh st) =
    show vl ++ "\n" ++ show bh ++ "\n" ++ show st

instance NFData (LedgerView era) where
  rnf (LedgerView _D _extraEntropy _pool _delegs _ccd) = ()

instance CryptoClass.Crypto c => NFData (BHeader c) where
  rnf (BHeader _ _) = ()

instance NFData (ChainDepState c) where
  rnf (ChainDepState _ _ _) = ()

instance NFData (ChainTransitionError c) where
  rnf _ = ()

instance CryptoClass.Crypto c => NFData (UpdateInputs c) where
  rnf (UpdateInputs g lv bh st) =
    seq (rnf g) (seq (rnf lv) (seq (rnf bh) (rnf st)))

genUpdateInputs ::
  forall era.
  ( EraGen era
  , Mock (EraCrypto era)
  , MinLEDGER_STS era
  , GetLedgerView era
  , EraRule "LEDGERS" era ~ API.ShelleyLEDGERS era
  , QC.HasTrace (API.ShelleyLEDGERS era) (GenEnv era)
  , PPUPState era ~ ShelleyPPUPState era
  , API.ApplyBlock era
  , EraTallyState era
  ) =>
  Int ->
  IO (UpdateInputs (EraCrypto era))
genUpdateInputs utxoSize = do
  let ge = genEnv (Proxy :: Proxy era)
  chainstate <- genChainState utxoSize ge
  (Block blockheader _) <- genBlock ge chainstate
  let ledgerview = currentLedgerView (chainNes chainstate)
  let (ChainState _newepochState keys eta0 etaV etaC etaH slot) = chainstate
  let prtclState = PrtclState keys eta0 etaV
  let ticknState = TicknState etaC etaH
  let nonce = case withOriginToMaybe slot of
        Just (LastAppliedBlock _blknum slotnum _hash) -> slotToNonce slotnum
        Nothing -> error "Empty Slot"
  pure
    ( UpdateInputs
        testGlobals
        ledgerview
        blockheader
        (ChainDepState prtclState ticknState nonce)
    )

updateChain ::
  (Mock c) =>
  UpdateInputs c ->
  Either (ChainTransitionError c) (ChainDepState c)
updateChain (UpdateInputs gl lv bh st) = updateChainDepState gl lv bh st

updateAndTickChain ::
  (Mock c) =>
  UpdateInputs c ->
  Either (ChainTransitionError c) (ChainDepState c)
updateAndTickChain (UpdateInputs gl lv bh st) =
  updateChainDepState gl lv bh
    . tickChainDepState gl lv True
    $ st
