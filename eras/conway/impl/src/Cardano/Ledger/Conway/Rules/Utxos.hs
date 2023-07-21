{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxos (ConwayUTXOS) where

import Cardano.Ledger.Alonzo.Language (Language (..))
import Cardano.Ledger.Alonzo.Rules (
  AlonzoUtxoEvent (..),
  AlonzoUtxoPredFailure (..),
  AlonzoUtxosEvent (..),
  AlonzoUtxosPredFailure (..),
  validBegin,
  validEnd,
 )
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.TxInfo (EraPlutusContext, ExtendedUTxO)
import Cardano.Ledger.Alonzo.UTxO (AlonzoScriptsNeeded)
import Cardano.Ledger.Babbage.Rules (
  BabbageUTXO,
  BabbageUtxoPredFailure (..),
  babbageEvalScriptsTxInvalid,
  expectScriptsToPass,
  tellDepositChangeEvent,
 )
import Cardano.Ledger.Babbage.Tx
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Block (txid)
import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Conway.Core
import Cardano.Ledger.Conway.Era (ConwayUTXOS)
import Cardano.Ledger.Conway.Governance (
  ConwayGovernance (..),
  GovernanceActionId (..),
  cgPropDepositsL,
  indexedGovProps,
 )
import Cardano.Ledger.Shelley.LedgerState (
  PPUPPredFailure,
  UTxOState (..),
  utxosGovernanceL,
 )
import Cardano.Ledger.Shelley.Rules (
  UtxoEnv (..),
  updateUTxOState,
 )
import Cardano.Ledger.UTxO (EraUTxO (..))
import Control.State.Transition.Extended
import Data.Foldable (Foldable (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as StrictSeq
import Debug.Trace (traceEvent)
import Lens.Micro

instance
  ( AlonzoEraTx era
  , ConwayEraPParams era
  , ConwayEraTxBody era
  , EraTxOut era
  , EraGovernance era
  , EraTxCert era
  , EraUTxO era
  , ExtendedUTxO era
  , EraPlutusContext 'PlutusV1 era
  , GovernanceState era ~ ConwayGovernance era
  , Script era ~ AlonzoScript era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (ConwayUTXOS era) ~ Tx era
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  ) =>
  STS (ConwayUTXOS era)
  where
  type BaseM (ConwayUTXOS era) = Cardano.Ledger.BaseTypes.ShelleyBase
  type Environment (ConwayUTXOS era) = UtxoEnv era
  type State (ConwayUTXOS era) = UTxOState era
  type Signal (ConwayUTXOS era) = AlonzoTx era
  type PredicateFailure (ConwayUTXOS era) = AlonzoUtxosPredFailure era
  type Event (ConwayUTXOS era) = AlonzoUtxosEvent era

  transitionRules = [utxosTransition]

instance
  ( AlonzoEraTx era
  , ConwayEraTxBody era
  , EraGovernance era
  , EraPlutusContext 'PlutusV1 era
  , EraTxOut era
  , EraTxCert era
  , EraUTxO era
  , ExtendedUTxO era
  , Event (EraRule "UTXOS" era) ~ AlonzoUtxosEvent era
  , GovernanceState era ~ ConwayGovernance era
  , PredicateFailure (EraRule "UTXOS" era) ~ AlonzoUtxosPredFailure era
  , Script era ~ AlonzoScript era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (ConwayUTXOS era) ~ Tx era
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  , ConwayEraPParams era
  ) =>
  Embed (ConwayUTXOS era) (BabbageUTXO era)
  where
  wrapFailed = AlonzoInBabbageUtxoPredFailure . UtxosFailure
  wrapEvent = UtxosEvent

utxosTransition ::
  forall era.
  ( AlonzoEraTx era
  , EraGovernance era
  , EraPlutusContext 'PlutusV1 era
  , EraUTxO era
  , ExtendedUTxO era
  , GovernanceState era ~ ConwayGovernance era
  , Script era ~ AlonzoScript era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (ConwayUTXOS era) ~ Tx era
  , Eq (PPUPPredFailure era)
  , Show (PPUPPredFailure era)
  , ConwayEraTxBody era
  , ConwayEraPParams era
  ) =>
  TransitionRule (ConwayUTXOS era)
utxosTransition =
  judgmentContext >>= \(TRC (_, _, tx)) -> do
    case tx ^. isValidTxL of
      IsValid True -> conwayEvalScriptsTxValid
      IsValid False -> babbageEvalScriptsTxInvalid

updateProposalDeposits ::
  (ConwayEraTxBody era, ConwayEraPParams era) =>
  PParams era ->
  TxBody era ->
  Map (GovernanceActionId (EraCrypto era)) Coin ->
  Map (GovernanceActionId (EraCrypto era)) Coin
updateProposalDeposits pp txb depMap = foldr' addPropDep depMap props
  where
    props =
      indexedGovProps . StrictSeq.fromStrict $
        txb ^. proposalProceduresTxBodyL
    addPropDep (idx, _) =
      Map.insert
        (GovernanceActionId (txid txb) idx)
        (pp ^. ppGovActionDepositL)

conwayEvalScriptsTxValid ::
  forall era.
  ( AlonzoEraTx era
  , EraUTxO era
  , EraPlutusContext 'PlutusV1 era
  , ExtendedUTxO era
  , Script era ~ AlonzoScript era
  , ScriptsNeeded era ~ AlonzoScriptsNeeded era
  , Signal (ConwayUTXOS era) ~ Tx era
  , STS (ConwayUTXOS era)
  , GovernanceState era ~ ConwayGovernance era
  , ConwayEraTxBody era
  , ConwayEraPParams era
  ) =>
  TransitionRule (ConwayUTXOS era)
conwayEvalScriptsTxValid = do
  TRC (UtxoEnv _ pp dpstate _, u@(UTxOState utxo _ _ gov _), tx) <-
    judgmentContext
  let txBody = tx ^. bodyTxL
  depositChange <- tellDepositChangeEvent pp dpstate txBody

  let !_ = traceEvent validBegin ()
  expectScriptsToPass pp tx utxo
  let !_ = traceEvent validEnd ()
  let !newUTxOState = updateUTxOState pp u txBody depositChange gov
  pure $
    newUTxOState
      & utxosGovernanceL . cgPropDepositsL %~ updateProposalDeposits pp txBody
