{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where
    import Data.Char

    newtype Score = Score Int
        deriving (Eq, Ord, Num, Show)

    instance Monoid Score where
        mempty = Score 0
        mappend = (+)

    score :: Num a => Char -> Score
    score char
        | upChar `elem` ['Q', 'Z'] = Score 10
        | upChar `elem` ['J', 'X'] = Score 8
        | upChar `elem` ['K'] = Score 5
        | upChar `elem` ['F', 'H', 'V', 'W', 'Y'] = Score 4
        | upChar `elem` ['B', 'C', 'M', 'P'] = Score 3
        | upChar `elem` ['D', 'G'] = Score 2
        | upChar `elem` ['A', 'E', 'I', 'L', 'N', 'O', 'R', 'S', 'T', 'U'] = Score 1
        | otherwise = Score 0
        where
            upChar = toUpper char

    scoreString :: String -> Score
    scoreString = (foldr (\c p -> p + (score c)) (Score 0))
