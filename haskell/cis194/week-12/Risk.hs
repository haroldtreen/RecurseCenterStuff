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

simulationRuns = 1000

maxDefenders = 2
maxAttackers = 3
minDefenders = 0
minAttackers = 1
minUnused = 1

traceRand :: Show a => Rand StdGen a -> Rand StdGen a
traceRand r = fmap traceShowId r

successProb :: Battlefield -> Rand StdGen Double
successProb bf = (fmap successCount battles) >>= (\count -> return (count / simulationRuns))
    where
        battles = sequence (map (\_ -> invade bf) [1..simulationRuns])
        isSuccess (Battlefield _ ds) = ds <= minDefenders
        successCount = foldl (\count b -> if isSuccess b then count + 1 else count) 0

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield as ds)
    | ds <= minDefenders = (return bf)
    | as <= minAttackers = (return bf)
    | otherwise = (battle bf) >>= invade

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = fmap newBattle casualties
    where
        attackingRolls = rollsForArmy $ getAttackingArmy bf
        defendingRolls = rollsForArmy $ getDefendingArmy bf
        casualties = getCasualties attackingRolls defendingRolls
        newBattle (aCas, dCas) = Battlefield (attackers bf - aCas) (defenders bf - dCas)

getCasualties :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen (Army, Army)
getCasualties attacks defenses = fmap foldMatches matches
    where
        matches = getMatches attacks defenses
        foldMatches = foldl (\tally match -> sumTuple tally (matchResult match)) (0, 0)

getMatches :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] -> Rand StdGen [(DieValue, DieValue)]
getMatches attacks defenses = zipFn <*> attacks <*> defenses
    where
        numMatches = liftM2 (\l1 l2 -> max (length l1) (length l2)) attacks defenses
        zipFn = fmap (zip .) (fmap take numMatches)

sumTuple :: Num a => (a, a) -> (a, a) -> (a, a)
sumTuple (a1, a2) (b1, b2) = (a1 + b1, a2 + b2)

matchResult :: (DieValue, DieValue) -> (Army, Army)
matchResult (attackVal, defenseVal)
    | attackVal > defenseVal = (0, 1)
    | otherwise = (1, 0)

rollsForArmy :: Army -> Rand StdGen [DieValue]
rollsForArmy army = fmap (reverse . sort) $ sequence (replicate army die)

getDefendingArmy :: Battlefield -> Army
getDefendingArmy bf = min (defenders bf) maxDefenders

getAttackingArmy :: Battlefield -> Army
getAttackingArmy bf = min (max 0 $ attackers bf - minUnused) maxDefenders
