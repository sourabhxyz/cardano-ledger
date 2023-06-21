{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Conway.Era (
  ConwayEra,
  ConwayCERT,
  ConwayDELEG,
  ConwayPOOL,
  ConwayGOVCERT,
  ConwayCERTS,
  ConwayGOV,
  ConwayNEWEPOCH,
  ConwayEPOCH,
  ConwayENACT,
  ConwayUTXO,
  ConwayUTXOS,
  ConwayUTXOW,
  ConwayTICKF,
  ConwayLEDGER,
  ConwayRATIFY,
) where

import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY)
import Cardano.Ledger.Babbage (BabbageEra)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Rules (
  ShelleyRUPD,
  ShelleySNAP,
  ShelleyTICK,
 )

-- =====================================================

-- | The Conway era
data ConwayEra c

instance Crypto c => Era (ConwayEra c) where
  type PreviousEra (ConwayEra c) = BabbageEra c
  type EraCrypto (ConwayEra c) = c
  type ProtVerLow (ConwayEra c) = 9

type instance Value (ConwayEra c) = MaryValue c

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

data ConwayGOV era

type instance EraRule "GOV" (ConwayEra c) = ConwayGOV (ConwayEra c)

data ConwayNEWEPOCH era

type instance EraRule "NEWEPOCH" (ConwayEra c) = ConwayNEWEPOCH (ConwayEra c)

data ConwayEPOCH era

type instance EraRule "EPOCH" (ConwayEra c) = ConwayEPOCH (ConwayEra c)

data ConwayENACT era

type instance EraRule "ENACT" (ConwayEra c) = ConwayENACT (ConwayEra c)

data ConwayUTXOS era

type instance EraRule "UTXOS" (ConwayEra c) = ConwayUTXOS (ConwayEra c)

data ConwayLEDGER era

type instance EraRule "LEDGER" (ConwayEra c) = ConwayLEDGER (ConwayEra c)

data ConwayTICKF era

type instance EraRule "TICKF" (ConwayEra c) = ConwayTICKF (ConwayEra c)

data ConwayRATIFY era

type instance EraRule "RATIFY" (ConwayEra c) = ConwayRATIFY (ConwayEra c)

data ConwayCERTS era

type instance EraRule "CERTS" (ConwayEra c) = ConwayCERTS (ConwayEra c)

data ConwayCERT era

type instance EraRule "CERT" (ConwayEra c) = ConwayCERT (ConwayEra c)

data ConwayDELEG era

type instance EraRule "DELEG" (ConwayEra c) = ConwayDELEG (ConwayEra c)

data ConwayPOOL era

type instance EraRule "POOL" (ConwayEra c) = ConwayPOOL (ConwayEra c)

data ConwayGOVCERT era

type instance EraRule "GOVCERT" (ConwayEra c) = ConwayGOVCERT (ConwayEra c)

data ConwayUTXOW era

type instance EraRule "UTXOW" (ConwayEra c) = ConwayUTXOW (ConwayEra c)

data ConwayUTXO era

type instance EraRule "UTXO" (ConwayEra c) = ConwayUTXO (ConwayEra c)

-- Rules inherited from Alonzo

type instance EraRule "BBODY" (ConwayEra c) = AlonzoBBODY (ConwayEra c)

-- Rules inherited from Shelley

type instance EraRule "LEDGERS" (ConwayEra c) = API.ShelleyLEDGERS (ConwayEra c)

type instance EraRule "POOLREAP" (ConwayEra c) = API.ShelleyPOOLREAP (ConwayEra c)

type instance EraRule "RUPD" (ConwayEra c) = ShelleyRUPD (ConwayEra c)

type instance EraRule "SNAP" (ConwayEra c) = ShelleySNAP (ConwayEra c)

type instance EraRule "TICK" (ConwayEra c) = ShelleyTICK (ConwayEra c)

-- =================================================
