{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE NumericUnderscores    #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module StakeContract
  ( stakingPlutusScript
  , stakingScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import           Codec.Serialise                ( serialise )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified PlutusTx.AssocMap              as AM
import qualified Plutus.V1.Ledger.Value         as Value
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           UsefulFuncs
{-
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-- the only allowed pool
poolId :: PlutusV2.PubKeyHash
poolId = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [224, 64, 146, 8, 1, 182, 157, 96, 193, 142, 43, 192, 1, 132, 101, 92, 137, 157, 222, 167, 171, 173, 92, 239, 69, 236, 220, 30] }

-- the payout address
payoutPkh :: PlutusV2.PubKeyHash
payoutPkh = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [56, 252, 183, 32, 94, 81, 223, 96, 51, 121, 131, 27, 0, 174, 120, 37, 247, 83, 227, 105, 252, 116, 145, 50, 127, 142, 119, 155] }

payoutSc :: PlutusV2.PubKeyHash
payoutSc = PlutusV2.PubKeyHash { PlutusV2.getPubKeyHash = createBuiltinByteString [64, 179, 201, 141, 59, 95, 90, 119, 72, 130, 157, 97, 46, 158, 28, 205, 95, 66, 153, 112, 171, 201, 171, 254, 174, 134, 122, 249] }

payoutAddr :: PlutusV2.Address
payoutAddr = createAddress payoutPkh payoutSc
-------------------------------------------------------------------------------
-- | Create the stake data.
-------------------------------------------------------------------------------
data StakeData = StakeData
  { stakeCred :: PlutusV2.ValidatorHash
  -- ^ The staking credential of the script.
  }
PlutusTx.unstableMakeIsData ''StakeData
-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Withdraw StakeData |
                          Delegate StakeData
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Withdraw, 0 )
                                                , ( 'Delegate, 1 )
                                                ]
-------------------------------------------------------------------------------
-- | mkPolicy :: Redeemer -> Context -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> PlutusV2.ScriptContext -> Bool
mkPolicy redeemer' context =
  case redeemer of
    -- handle the withdrawl of staking rewards
    (Withdraw sd) -> do
      { let stakingCred = PlutusV2.StakingHash  $ PlutusV2.ScriptCredential $ stakeCred sd
      ; let a = traceIfFalse "Bad Withdrawal"   $ checkTheWithdrawal rewardWithdrawal stakingCred
      ;         traceIfFalse "Withdrawal Error" $ all (==True) [a]
      }
    
    -- handle the pool delegation
    (Delegate sd) -> do
      { let stakingCred = PlutusV2.StakingHash $ PlutusV2.ScriptCredential $ stakeCred sd
      ; let a = traceIfFalse "Bad Deleg Cert"  $ checkTheCerts dCerts stakingCred
      ;         traceIfFalse "Delegate Error"  $ all (==True) [a]
      }
    
  where
    redeemer :: CustomRedeemerType
    redeemer = PlutusTx.unsafeFromBuiltinData @CustomRedeemerType redeemer' -- build out the data type
    
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo context

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = PlutusV2.txInfoOutputs info

    dCerts :: [PlutusV2.DCert]
    dCerts = PlutusV2.txInfoDCert info

    rewardWithdrawal :: [(PlutusV2.StakingCredential, Integer)]
    rewardWithdrawal = AM.toList $ PlutusV2.txInfoWdrl info

     -- | check for withdraws then check if the payout address gets the reward
    checkTheWithdrawal :: [(PlutusV2.StakingCredential, Integer)] -> PlutusV2.StakingCredential -> Bool
    checkTheWithdrawal []     _  = False
    checkTheWithdrawal (x:xs) sc =
      if     traceIfFalse "Incorrect Stake Key"      $ stakeCred == sc                                           -- must be from this stake
        then traceIfFalse "Incorrect Reward Payment" $ isAddrGettingPaidExactly txOutputs payoutAddr payoutValue -- send reward to payout address
        else checkTheWithdrawal xs sc
      where
        stakeCred :: PlutusV2.StakingCredential
        stakeCred = fst x

        rewardAmt :: Integer
        rewardAmt = snd x

        payoutValue :: PlutusV2.Value
        payoutValue = adaValue rewardAmt

    -- | loop all the certs and check if the stake is going to the right pool
    checkTheCerts :: [PlutusV2.DCert] -> PlutusV2.StakingCredential -> Bool
    checkTheCerts []     _  = False
    checkTheCerts (x:xs) sc =
      if checkCert x
        then True                -- correct credential and pool
        else checkTheCerts xs sc -- loop all the certs
      where
        checkCert :: PlutusV2.DCert -> Bool
        checkCert cert = 
          case cert of
            -- check for a delegation to stake pool
            (PlutusV2.DCertDelegDelegate sc' poolId') -> 
              ( traceIfFalse "Incorrect Stake Key" $ sc     == sc'     ) && -- only this cred can be staked
              ( traceIfFalse "Incorrect Pool Id"   $ poolId == poolId' )    -- must delegate to specific pool id
    
            -- any other cert fails but stake registration
            _ -> False                                              -- any other cert fails but not registration
-------------------------------------------------------------------------------
-- | Compile Information
-------------------------------------------------------------------------------
policy :: PlutusV2.StakeValidator
policy = PlutusV2.mkStakeValidatorScript $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Utils.mkUntypedStakeValidator mkPolicy

plutusScript :: Scripts.Script
plutusScript = PlutusV2.unStakeValidatorScript policy

validator :: PlutusV2.Validator
validator = PlutusV2.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise validator

stakingPlutusScript :: PlutusScript PlutusScriptV2
stakingPlutusScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

stakingScriptShortBs :: SBS.ShortByteString
stakingScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor