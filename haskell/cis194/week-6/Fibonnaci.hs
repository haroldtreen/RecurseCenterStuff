module Fibonnaci where
    import Data.List

    -- Exercise 1

    fib :: Integer -> Integer
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n - 1) + fib (n - 2)

    fib1 :: [Integer]
    fib1 = map fib [0..]

    -- Exercise 2

    fib2 :: [Integer]
    fib2 = 0 : 1 : [ fib2 !! (x - 1) + fib2 !! (x - 2) | x <- [2..] ]

    -- Exercise 3

    data Stream a = Item a (Stream a)
    instance Show a => Show (Stream a) where
        show = show . take 20 . streamToList

    streamToList :: Stream a -> [a]
    streamToList (Item a stream) = a : streamToList stream

    -- Exercise 4

    streamRepeat :: a -> Stream a
    streamRepeat a = Item a (streamRepeat a)

    streamMap :: (a -> b) -> Stream a -> Stream b
    streamMap fn (Item a stream) = Item (fn a) (streamMap fn stream)

    streamFromSeed :: (a -> a) -> a -> Stream a
    streamFromSeed fn seed = Item (seed) (streamFromSeed fn (fn seed))

    -- Exercise 5

    nats :: Stream Integer
    nats = streamFromSeed (+1) 0

    interleaveStreams :: Stream a -> Stream a -> Stream a
    interleaveStreams (Item a streamA) streamB = Item a (interleaveStreams streamB streamA)

    buildRuler :: Integer -> Stream Integer
    buildRuler x = interleaveStreams (streamRepeat x) (buildRuler (x + 1))

    ruler :: Stream Integer
    ruler = buildRuler 0
