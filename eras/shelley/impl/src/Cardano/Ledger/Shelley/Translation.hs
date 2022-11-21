{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}
module Cardano.Ledger.Shelley.Translation where

import Cardano.Ledger.Shelley.Era (ShelleyEra)
import Cardano.Ledger.Core (TranslationContext, Era, Crypto)
import Cardano.Ledger.Shelley.PParams (ShelleyPParams, emptyPParams)
import Cardano.Ledger.Shelley.Genesis (ShelleyGenesis (..))
import Cardano.Ledger.Keys
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import GHC.Word (Word64)
import NoThunks.Class (NoThunks (..))

-- | Required data to translate a Byron ledger into a Shelley ledger.
data FromByronTranslationContext era = FromByronTranslationContext {
    fbtcGenDelegs         :: !(Map (KeyHash 'Genesis (Crypto era)) (GenDelegPair (Crypto era)))
  , fbtcProtocolParams    :: !(ShelleyPParams era)
  , fbtcMaxLovelaceSupply :: !Word64
  } deriving (Eq, Show, Generic)

-- | Trivial FromByronTranslationContext value, for use in cases where we do not need
-- to translate from Byron to Shelley.
emptyFromByronTranslationContext :: FromByronTranslationContext era
emptyFromByronTranslationContext = FromByronTranslationContext {
    fbtcGenDelegs = Map.empty
  , fbtcMaxLovelaceSupply = 0
  , fbtcProtocolParams = emptyPParams
  }

toFromByronTranslationContext :: ShelleyGenesis era -> FromByronTranslationContext era
toFromByronTranslationContext ShelleyGenesis {sgGenDelegs, sgMaxLovelaceSupply, sgProtocolParams} =
  FromByronTranslationContext {
     fbtcGenDelegs = sgGenDelegs
   , fbtcProtocolParams = sgProtocolParams
   , fbtcMaxLovelaceSupply = sgMaxLovelaceSupply
   }

deriving instance Era era => NoThunks (FromByronTranslationContext era)

type instance TranslationContext (ShelleyEra c) = FromByronTranslationContext (ShelleyEra c)