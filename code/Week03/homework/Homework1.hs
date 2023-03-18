{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                       ScriptContext, TxInfo, Validator, scriptContextTxInfo, txInfoValidRange,
                                       mkValidatorScript)
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..), ($), (&&), (||), (+))
import           Utilities            (wrap)


import           Plutus.V2.Ledger.Contexts (txSignedBy)
import           Plutus.V1.Ledger.Interval (contains, to, from)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx = (txSignerCheck (beneficiary1 _dat) && beforeDeadline) || (txSignerCheck (beneficiary2 _dat) && afterDeadline)
    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx

        txSignerCheck :: PubKeyHash -> Bool
        txSignerCheck = txSignedBy info

        beforeDeadline :: Bool
        beforeDeadline = contains (to $ deadline _dat) $ txInfoValidRange info

        afterDeadline :: Bool
        afterDeadline = contains (from (deadline _dat + 1)) $ txInfoValidRange info

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrap mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
