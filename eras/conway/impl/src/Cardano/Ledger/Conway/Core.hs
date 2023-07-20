{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Cardano.Ledger.Conway.Core (
  module X,
  ConwayEraTxBody (..),
)
where

import Cardano.Ledger.Babbage.Core as X
import Cardano.Ledger.Conway.Governance.Procedures (ProposalProcedure, VotingProcedures)
import Data.Sequence.Strict (StrictSeq)
import Lens.Micro (Lens')

class BabbageEraTxBody era => ConwayEraTxBody era where
  votingProceduresTxBodyL :: Lens' (TxBody era) (VotingProcedures era)
  proposalProceduresTxBodyL :: Lens' (TxBody era) (StrictSeq (ProposalProcedure era))
