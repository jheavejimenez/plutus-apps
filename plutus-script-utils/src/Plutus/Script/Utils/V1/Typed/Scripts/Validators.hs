{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module Plutus.Script.Utils.V1.Typed.Scripts.Validators
    ( UntypedValidator
    , mkUntypedValidator
    ---
    , ValidatorTypes(..)
    , ValidatorType
    , TypedValidator
    , mkTypedValidator
    , mkTypedValidatorParam
    , validatorHash
    , validatorAddress
    , validatorScript
    , unsafeMkTypedValidator
    , forwardingMintingPolicy
    , forwardingMintingPolicyHash
    , generalise
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Void (Void)
import GHC.Generics (Generic)

import Plutus.V1.Ledger.Address qualified as PV1
import Plutus.V1.Ledger.Api qualified as PV1

import PlutusCore.Default (DefaultUni)
import PlutusTx (BuiltinData, CompiledCode, Lift, UnsafeFromData (unsafeFromBuiltinData), applyCode, liftCode)

import Plutus.Script.Utils.V1.Scripts qualified
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies qualified as MPS
import Plutus.Script.Utils.V1.Typed.TypeUtils (Any)

import PlutusTx.Prelude (check)

type UntypedValidator = BuiltinData -> BuiltinData -> BuiltinData -> ()

{-# INLINABLE mkUntypedValidator #-}
-- | Converts a custom datum and redeemer from a validator function to an
-- untyped validator function. See Note [Scripts returning Bool].
--
-- Here's an example of how this function can be used:
--
-- @
--   import PlutusTx qualified
--   import Plutus.V1.Ledger.Scripts qualified as Plutus
--
--   newtype MyCustomDatum = MyCustomDatum Integer
--   PlutusTx.unstableMakeIsData ''MyCustomDatum
--   newtype MyCustomRedeemer = MyCustomRedeemer Integer
--   PlutusTx.unstableMakeIsData ''MyCustomRedeemer
--
--   mkValidator :: MyCustomDatum -> MyCustomRedeemer -> Plutus.ScriptContext -> Bool
--   mkValidator _ _ _ = True
--
--   validator :: Plutus.Validator
--   validator = Plutus.mkValidatorScript
--       $$(PlutusTx.compile [|| wrap ||])
--    where
--       wrap = mkUntypedValidator mkValidator
-- @
mkUntypedValidator
    :: forall d r
    . (UnsafeFromData d, UnsafeFromData r)
    => (d -> r -> PV1.ScriptContext -> Bool)
    -> UntypedValidator
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
mkUntypedValidator f d r p =
    check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)

-- | A class that associates a type standing for a connection type with two types, the type of the
-- redeemer and the data script for that connection type.
class ValidatorTypes (a :: Type) where
    -- | The type of the redeemers of this connection type.
    type RedeemerType a :: Type
    -- | The type of the data of this connection type.
    type DatumType a :: Type

    -- Defaults
    type instance RedeemerType a = ()
    type instance DatumType  a = ()

-- | The type of validators for the given connection type.
type ValidatorType (a :: Type) = DatumType a -> RedeemerType a -> PV1.ScriptContext -> Bool

instance ValidatorTypes Void where
    type RedeemerType Void = Void
    type DatumType Void = Void

instance ValidatorTypes Any where
    type RedeemerType Any = BuiltinData
    type DatumType Any = BuiltinData

-- | A typed validator script with its 'ValidatorScript' and 'Address'.
data TypedValidator (a :: Type) =
    TypedValidator
        { tvValidator         :: PV1.Validator
        , tvValidatorHash     :: PV1.ValidatorHash
        , tvForwardingMPS     :: PV1.MintingPolicy
        , tvForwardingMPSHash :: PV1.MintingPolicyHash
        -- ^ The hash of the minting policy that checks whether the validator
        --   is run in this transaction
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

{-| Generalise the typed validator to one that works with the 'Data' type.
-}
generalise :: forall a. TypedValidator a -> TypedValidator Any
generalise TypedValidator{tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash} =
    -- we can do this safely because the on-chain validators are untyped, so they always
    -- take 'BuiltinData' arguments. The validator script stays the same, so the conversion
    -- from 'BuiltinData' to 'a' still takes place, even if it's not reflected in the type
    -- signature anymore.
    TypedValidator{tvValidator, tvValidatorHash, tvForwardingMPS, tvForwardingMPSHash}

-- | Make a 'TypedValidator' from the 'CompiledCode' of a validator script and its wrapper.
mkTypedValidator ::
    CompiledCode (ValidatorType a)
    -- ^ Validator script (compiled)
    -> CompiledCode (ValidatorType a -> UntypedValidator)
    -- ^ A wrapper for the compiled validator
    -> TypedValidator a
mkTypedValidator vc wrapper =
    let val = PV1.mkValidatorScript $ wrapper `applyCode` vc
        hsh = Plutus.Script.Utils.V1.Scripts.validatorHash val
        mps = MPS.mkForwardingMintingPolicy hsh
    in TypedValidator
        { tvValidator         = val
        , tvValidatorHash     = hsh
        , tvForwardingMPS     = mps
        , tvForwardingMPSHash = Plutus.Script.Utils.V1.Scripts.mintingPolicyHash mps
        }

-- | Make a 'TypedValidator' from the 'CompiledCode' of a parameterized validator script and its wrapper.
mkTypedValidatorParam
    :: forall a param. Lift DefaultUni param
    => CompiledCode (param -> ValidatorType a)
    -- ^ Validator script (compiled)
    -> CompiledCode (ValidatorType a -> UntypedValidator)
    -- ^ A wrapper for the compiled validator
    -> param
    -- ^ The extra paramater for the validator script
    -> TypedValidator a
mkTypedValidatorParam vc wrapper param =
    mkTypedValidator (vc `PlutusTx.applyCode` PlutusTx.liftCode param) wrapper

-- | The hash of the validator.
validatorHash :: TypedValidator a -> PV1.ValidatorHash
validatorHash = tvValidatorHash

-- | The address of the validator.
validatorAddress :: TypedValidator a -> PV1.Address
validatorAddress = PV1.scriptHashAddress . tvValidatorHash

-- | The validator script itself.
validatorScript :: TypedValidator a -> PV1.Validator
validatorScript = tvValidator

-- | Make a 'TypedValidator' (with no type constraints) from an untyped 'Validator' script.
unsafeMkTypedValidator :: PV1.Validator -> TypedValidator Any
unsafeMkTypedValidator vl =
    let vh = Plutus.Script.Utils.V1.Scripts.validatorHash vl
        mps = MPS.mkForwardingMintingPolicy vh
    in
    TypedValidator
        { tvValidator         = vl
        , tvValidatorHash     = vh
        , tvForwardingMPS     = mps
        , tvForwardingMPSHash = Plutus.Script.Utils.V1.Scripts.mintingPolicyHash mps
        }

-- | The minting policy that forwards all checks to the instance's
--   validator
forwardingMintingPolicy :: TypedValidator a -> PV1.MintingPolicy
forwardingMintingPolicy = tvForwardingMPS

-- | Hash of the minting policy that forwards all checks to the instance's
--   validator
forwardingMintingPolicyHash :: TypedValidator a -> PV1.MintingPolicyHash
forwardingMintingPolicyHash = tvForwardingMPSHash
