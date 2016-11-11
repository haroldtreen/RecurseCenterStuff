{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where
    import ExprT
    import Parser
    import StackVM

    -- Exercise 1

    eval :: ExprT -> Integer
    eval (ExprT.Lit num) = num
    eval (ExprT.Add expr1 expr2) = eval expr1 + eval expr2
    eval (ExprT.Mul expr1 expr2) = eval expr1 * eval expr2

    -- Exercise 2

    evalStr :: String -> Maybe Integer
    evalStr str = case parseExp ExprT.Lit ExprT.Add ExprT.Mul str of
        Nothing -> Nothing
        Just expr -> Just (eval expr)
        where
            expr = parseExp ExprT.Lit ExprT.Add ExprT.Mul str

    -- Exercise 3

    class Expr a where
        lit :: Integer -> a
        add :: a -> a -> a
        mul :: a -> a -> a

    instance Expr ExprT where
        lit = ExprT.Lit
        add = ExprT.Add
        mul = ExprT.Mul

    -- Exercise 4

    instance Expr Integer where
        lit = id
        add = (+)
        mul = (*)

    instance Expr Bool where
        lit a = a >= 0
        add = (||)
        mul = (&&)

    newtype MinMax = MinMax Integer deriving (Eq, Show)
    newtype Mod7 = Mod7 Integer deriving (Eq, Show)

    instance Expr MinMax where
        lit a = MinMax a
        add (MinMax a) (MinMax b) = MinMax (max a b)
        mul (MinMax a) (MinMax b) = MinMax (min a b)

    instance Expr Mod7 where
        lit a = Mod7 (a `mod` 7)
        add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
        mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)

    testExp :: Expr a => Maybe a
    testExp = parseExp lit add mul "(3 * -4) + 5"

    testInteger = testExp :: Maybe Integer
    testBool = testExp :: Maybe Bool
    testMM = testExp :: Maybe MinMax
    testSat = testExp :: Maybe Mod7

    -- Exercise 5

    instance Expr Program where
        lit a = [PushI a]
        add a b = a ++ b ++ [StackVM.Add]
        mul a b = a ++ b ++ [StackVM.Mul]

    compile :: String -> Maybe Program
    compile = parseExp lit add mul
