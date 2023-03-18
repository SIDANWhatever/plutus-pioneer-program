{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, Validator, TxInfo, scriptContextTxInfo, txInfoValidRange,
                                       mkValidatorScript)
import           PlutusTx             (applyCode, compile, liftCode)
import           PlutusTx.Prelude     (Bool (..), (.), ($), (&&))
import           Utilities            (wrap)

import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           Plutus.V1.Ledger.Interval (contains, from)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator _beneficiary _deadline () _ctx = txSignerCheck _beneficiary && afterDeadline
    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx

        txSignerCheck :: PubKeyHash -> Bool
        txSignerCheck = txSignedBy info

        afterDeadline :: Bool
        afterDeadline = contains (from _deadline) $ txInfoValidRange info

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrap . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)
