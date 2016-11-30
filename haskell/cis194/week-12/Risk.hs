{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List
import Debug.Trace

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving (Show)

simulationRuns = 1000 :: Int

maxDefenders = 2
maxAttackers = 3
minDefenders = 0
minAttackers = 1

traceRand :: Show a => Rand StdGen a -> Rand StdGen a
traceRand r = fmap traceShowId r

dice :: Int -> Rand StdGen [DieValue]
dice n = replicateM n die

successProb :: Battlefield -> Rand StdGen Double
successProb bf = battles >>=
    (\battles -> return $ successCount battles) >>=
        (\count -> return $ count / fromIntegral simulationRuns)
    where
        battles = replicateM simulationRuns (invade bf)
        isSuccess (Battlefield _ ds) = ds <= minDefenders
        successCount = foldl (\count b -> if isSuccess b then count + 1 else count) 0

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield as ds)
    | ds <= minDefenders || as <= minAttackers = (return bf)
    | otherwise = (battle bf) >>= invade

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield as ds) = dice (fst armyTup + snd armyTup) >>=
    (\dVs -> return $ deaths $ splitDice (armyTup) dVs) >>=
        (\(aDeaths, dDeaths) -> return $ Battlefield (as - aDeaths) (ds - dDeaths))
        where armyTup = armies bf

deaths :: [(DieValue, DieValue)] -> (Army, Army)
deaths dVPairs = foldl compareDie (0, 0) dVPairs
    where compareDie (aDeaths, dDeaths) (aDie, dDie)
            | aDie > dDie = (aDeaths, dDeaths + 1)
            | otherwise = (aDeaths + 1, dDeaths)

splitDice :: (Army, Army) -> [DieValue] -> [(DieValue, DieValue)]
splitDice (attack, defense) dVs = zip attackDice defenseDice
    where
        attackDice = reverse . sort $ take attack dVs
        defenseDice = reverse . sort $ drop attack dVs

armies :: Battlefield -> (Army, Army)
armies bf = (attack, defense)
    where
        defense = min (defenders bf) maxDefenders
        attack = min (max 0 $ attackers bf - minAttackers) maxDefenders
