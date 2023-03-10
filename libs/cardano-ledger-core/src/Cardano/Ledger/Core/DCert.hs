{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Cardano.Ledger.Core.DCert (
  EraDCert (..),
  Delegation (..),
  DelegCert (..),
  delegCWitness,
  PoolCert (..),
  poolCWitness,
  ConstitutionalDelegCert (..),
  genesisCWitness,
)
where

import Cardano.Ledger.Binary (DecCBOR, EncCBOR, FromCBOR, ToCBOR)
import Cardano.Ledger.Core.Era
import Cardano.Ledger.Credential (Credential (..), StakeCredential)
import Cardano.Ledger.Keys (
  Hash,
  KeyHash (..),
  KeyRole (..),
  VerKeyVRF,
 )
import Cardano.Ledger.PoolParams
import Cardano.Ledger.Slot (EpochNo (..))
import Control.DeepSeq (NFData (..), rwhnf)
import Data.Kind (Type)
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

class
  ( DecCBOR (DCert era)
  , EncCBOR (DCert era)
  , ToCBOR (DCert era)
  , FromCBOR (DCert era)
  , NoThunks (DCert era)
  , NFData (DCert era)
  , Show (DCert era)
  , Eq (DCert era)
  ) =>
  EraDCert era
  where
  type DCert era = (r :: Type) | r -> era

  getDCertDeleg :: DCert era -> Maybe (DelegCert (EraCrypto era))

  getDCertPool :: DCert era -> Maybe (PoolCert (EraCrypto era))

  getDCertGenesis :: DCert era -> Maybe (ConstitutionalDelegCert (EraCrypto era))

-- | The delegation of one stake key to another.
data Delegation c = Delegation
  { dDelegator :: !(StakeCredential c)
  , dDelegatee :: !(KeyHash 'StakePool c)
  }
  deriving (Eq, Generic, Show)

instance NFData (Delegation c) where
  rnf = rwhnf

instance NoThunks (Delegation c)

data DelegCert c
  = -- | A stake key registration certificate.
    RegKey !(StakeCredential c)
  | -- | A stake key deregistration certificate.
    DeRegKey !(StakeCredential c)
  | -- | A stake delegation certificate.
    Delegate !(Delegation c)
  deriving (Show, Generic, Eq)

instance NoThunks (DelegCert c)

instance NFData (DelegCert c) where
  rnf = rwhnf

data PoolCert c
  = -- | A stake pool registration certificate.
    RegPool !(PoolParams c)
  | -- | A stake pool retirement certificate.
    RetirePool !(KeyHash 'StakePool c) !EpochNo
  deriving (Show, Generic, Eq)

instance NoThunks (PoolCert c)

instance NFData (PoolCert c) where
  rnf = rwhnf

-- | Constitutional key delegation certificate
data ConstitutionalDelegCert c
  = ConstitutionalDelegCert
      !(KeyHash 'Genesis c)
      !(KeyHash 'GenesisDelegate c)
      !(Hash c (VerKeyVRF c))
  deriving (Show, Generic, Eq)

instance NoThunks (ConstitutionalDelegCert c)

instance NFData (ConstitutionalDelegCert c) where
  rnf = rwhnf

-- | Determine the certificate author
delegCWitness :: DelegCert c -> Credential 'Staking c
delegCWitness (RegKey _) = error "no witness in key registration certificate"
delegCWitness (DeRegKey hk) = hk
delegCWitness (Delegate delegation) = dDelegator delegation

poolCWitness :: PoolCert c -> Credential 'StakePool c
poolCWitness (RegPool pool) = KeyHashObj $ ppId pool
poolCWitness (RetirePool k _) = KeyHashObj k

genesisCWitness :: ConstitutionalDelegCert c -> KeyHash 'Genesis c
genesisCWitness (ConstitutionalDelegCert gk _ _) = gk
