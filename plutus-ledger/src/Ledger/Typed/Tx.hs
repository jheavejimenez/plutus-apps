{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE NamedFieldPuns   #-}

-- | Typed transaction inputs and outputs. This module defines typed versions
--   of various ledger types. The ultimate goal is to make sure that the script
--   types attached to inputs and outputs line up, to avoid type errors at
--   validation time.
module Ledger.Typed.Tx
  ( module Ledger.Typed.Tx
  , module Plutus.Script.Utils.V1.Typed.Scripts
  ) where

import Control.Lens (preview)
import Control.Monad.Except (MonadError, throwError)
import Ledger.Tx (ChainIndexTxOut, TxIn (TxIn, txInRef, txInType), TxInType (ConsumeScriptAddress), TxOutRef,
                  _ScriptChainIndexTxOut)
import Plutus.Script.Utils.V1.Typed.Scripts
import PlutusTx (FromData, ToData)

-- | Create a 'TypedScriptTxIn' from an existing 'TxIn' by checking the types of its parts.
typeScriptTxIn
    :: forall inn m
    . ( FromData (RedeemerType inn)
      , ToData (RedeemerType inn)
      , FromData (DatumType inn)
      , ToData (DatumType inn)
      , MonadError ConnectionError m)
    => (TxOutRef -> Maybe ChainIndexTxOut)
    -> TypedValidator inn
    -> TxIn
    -> m (TypedScriptTxIn inn)
typeScriptTxIn lookupRef si TxIn{txInRef,txInType} = do
    (rs, ds) <- case txInType of
        Just (ConsumeScriptAddress _ rs ds) -> pure (rs, ds)
        Just x                              -> throwError $ WrongInType x
        Nothing                             -> throwError MissingInType
    -- It would be nice to typecheck the validator script here (we used to do that when we
    -- had typed on-chain code), but we can't do that with untyped code!
    rsVal <- checkRedeemer si rs
    _ <- checkDatum si ds
    typedOut <- typeScriptTxOutRef @inn lookupRef si txInRef
    pure $ makeTypedScriptTxIn si rsVal typedOut

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOut
    :: forall out m
    . ( FromData (DatumType out)
      , ToData (DatumType out)
      , MonadError ConnectionError m)
    => TypedValidator out
    -> TxOutRef
    -> ChainIndexTxOut
    -> m (TypedScriptTxOut out)
typeScriptTxOut si ref txout = do
    (addr, datum, outVal) <- case preview _ScriptChainIndexTxOut txout of
        Just (addr,_ ,datum, outVal) -> pure (addr, datum, outVal)
        _                            -> throwError $ WrongOutType ExpectedScriptGotPubkey

    ds <- case datum of
      Left dsh -> throwError $ NoDatum ref dsh
      Right ds -> pure ds
    checkValidatorAddress si addr
    dsVal <- checkDatum si ds
    pure $ makeTypedScriptTxOut si dsVal outVal

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts. To do this we
-- need to cross-reference against the validator script and be able to look up the 'TxOut' to which this
-- reference points.
typeScriptTxOutRef
    :: forall out m
    . ( FromData (DatumType out)
      , ToData (DatumType out)
      , MonadError ConnectionError m)
    => (TxOutRef -> Maybe ChainIndexTxOut)
    -> TypedValidator out
    -> TxOutRef
    -> m (TypedScriptTxOutRef out)
typeScriptTxOutRef lookupRef ct ref = do
    out <- case lookupRef ref of
        Just res -> pure res
        Nothing  -> throwError UnknownRef
    tyOut <- typeScriptTxOut @out ct ref out
    pure $ TypedScriptTxOutRef ref tyOut
