{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}


{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Data.Void                 (Void)
import           Control.Monad             (void)
import qualified Data.Map                  as Map
import qualified Data.ByteString.Char8     as C
import           Playground.Contract
import qualified PlutusTx                  as PlutusTx
import           PlutusTx.Prelude 

import           Plutus.Contract
import           Ledger
import           Ledger.Constraints        (TxConstraints)
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Ada                as Ada
import           Playground.Contract
import qualified Prelude
import Prelude   (Show (..), String)
import           Text.Printf          (printf)

------------------------------------------------------------
-- | On-Chain code
------------------------------------------------------------

data BetDatum = BetDatum
    { bDeadline :: POSIXTime
    , bBeneficiaryTaker :: PaymentPubKeyHash
    , bBeneficiaryPlacer :: PaymentPubKeyHash
    } deriving (Prelude.Eq, Prelude.Show)

PlutusTx.unstableMakeIsData ''BetDatum

{-# INLINABLE validateBet #-}
validateBet :: BetDatum -> Integer -> ScriptContext -> Bool
validateBet pDat x ctx =
  traceIfFalse "Wrong Guess" $ x == randomNum &&
  traceIfFalse "Betting start time is not reached" deadlineReached &&
  traceIfFalse "beneficiary's taker signature missing" signedByBeneficiaryTaker || -- If both beneficiary false than validate will be false
  traceIfFalse "beneficiary's Placer signature missing" signedByBeneficiaryPlacer

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    deadlineReached :: Bool
    deadlineReached = contains (from $ bDeadline pDat) $ txInfoValidRange info

    signedByBeneficiaryTaker :: Bool
    signedByBeneficiaryTaker = txSignedBy info $ unPaymentPubKeyHash $ bBeneficiaryTaker pDat

    signedByBeneficiaryPlacer :: Bool
    signedByBeneficiaryPlacer = txSignedBy info $ unPaymentPubKeyHash $ bBeneficiaryPlacer pDat

-- | Datum and redeemer parameter types
data Betting
instance Scripts.ValidatorTypes Betting where
    type instance RedeemerType Betting = Integer
    type instance DatumType Betting = BetDatum

-- | The script instance is the compiled validator (ready to go onto the chain)
betInstance :: Scripts.TypedValidator Betting
betInstance = Scripts.mkTypedValidator @Betting
  $$(PlutusTx.compile [|| validateBet ||])
  $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @BetDatum @Integer

validator :: Validator
validator = Scripts.validatorScript betInstance

-- | Off-Chain code
betAddress :: Address
betAddress = Ledger.scriptAddress (Scripts.validatorScript betInstance)

-- | Parameters for the "place" endpoint
data PlaceParams = PlaceParams
    { ppAmount    :: !Value
    , ppBeneficiaryTaker :: !PaymentPubKeyHash -- Target's wallet addr (Taker)
    }
    deriving (Prelude.Eq, Prelude.Show, Generic, FromJSON, ToJSON, ToSchema)

--  | Parameters for the "take" endpoint
data TakeParams = TakeParams
    { tpAmount    :: !Value
    , tpBeneficiaryPlacer :: !PaymentPubKeyHash -- Placer's wallet addr
    , tpStartFrom :: !POSIXTime
    }
    deriving (Prelude.Eq, Prelude.Show, Generic, FromJSON, ToJSON, ToSchema)

--  | Parameters for the "Check Out" endpoint
data GuessParams = GuessParams
    { yourGuess :: !Integer
    }
    deriving (Prelude.Eq, Prelude.Show, Generic, FromJSON, ToJSON, ToSchema, ToArgument)

type BettingSchema =
            Endpoint "1.PlaceBet" PlaceParams
        .\/ Endpoint "2.TakeBet" TakeParams
        .\/ Endpoint "3.Guess" GuessParams

-- | The "PlaceBet" contract endpoint.
place :: AsContractError e => PlaceParams -> Contract () BettingSchema e ()
place pp = do
    pkh   <- ownPaymentPubKeyHash -- Need to send ADA when taker loss
    
    let datum = BetDatum 
                { bDeadline = 1596059092999 -- +0 slots, default value of deadline
                , bBeneficiaryTaker = ppBeneficiaryTaker pp
                , bBeneficiaryPlacer = pkh
                }

        tx   = Constraints.mustPayToTheScript datum (ppAmount pp)
        
    ledgerTx <- submitTxConstraints betInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a bet of %s lovalace to taker's beneficiary %s" 
        (show $ ppAmount pp)
        (show $ ppBeneficiaryTaker pp)

-- | The "takeBet" contract endpoint.
takeBet :: AsContractError e => TakeParams -> Contract () BettingSchema e ()
takeBet tp = do
    pkh   <- ownPaymentPubKeyHash
    let datum = BetDatum 
                { bDeadline = tpStartFrom tp
                , bBeneficiaryTaker = pkh
                , bBeneficiaryPlacer = tpBeneficiaryPlacer tp
                }

        tx   = Constraints.mustPayToTheScript datum (tpAmount tp)
    
    ledgerTx <- submitTxConstraints betInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a take tx of %s lovalace to beneficiary placerr %s" 
        (show $ tpAmount tp)
        (show $ tpBeneficiaryPlacer tp)

randomNum = 2 -- random Number still hardcoded

-- | The "guess" contract endpoint.
guess :: AsContractError e => GuessParams -> Contract () BettingSchema e ()
guess (GuessParams guessValue) = do
    now   <- currentTime  
    pkh   <- ownPaymentPubKeyHash

    -- filter all incorrect datum scripts
    unspentOutputs <- Map.filter (isSuitable now pkh) <$> utxosAt betAddress

    let redeemer = Redeemer $ PlutusTx.toBuiltinData guessValue

    if Map.null unspentOutputs
        then logInfo @String $ "no bet available"
        else do
            logInfo @String $ "bet are available"

            let tx = collectFromScript unspentOutputs guessValue
                  <> Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsSpending betInstance unspentOutputs tx

            logInfo @String $ "after submiting"            
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "Reward Collected"
     
    where
    isSuitable ::  POSIXTime -> PaymentPubKeyHash -> ChainIndexTxOut -> Bool
    isSuitable now pkh o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            Just d  -> bBeneficiaryTaker d == pkh || bBeneficiaryPlacer d == pkh && bDeadline d <= now && randomNum == guessValue

-- | Betting endpoints.
endpoints :: AsContractError e => Contract () BettingSchema e ()
endpoints = awaitPromise (place' `select` take' `select` guess') >> endpoints
  where
    place' = endpoint @"1.PlaceBet" place
    take' = endpoint @"2.TakeBet" takeBet
    guess' = endpoint @"3.Guess" guess

mkSchemaDefinitions ''BettingSchema
