-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

toDigits :: Integer -> [Integer]
toDigits n
    | n == 0 = []
    | n <= 9 = [n]
    | otherwise = toDigits(n `quot` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse(toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse (doubleEveryOtherFront (reverse n))

doubleEveryOtherFront :: [Integer] -> [Integer]
doubleEveryOtherFront [] = []
doubleEveryOtherFront (x:[]) = [x]
doubleEveryOtherFront (first:other:rest) = [first, other * 2] ++ doubleEveryOtherFront rest

splitDigits :: [Integer] -> [Integer]
splitDigits [] = []
splitDigits (x:xs) = toDigits x ++ splitDigits xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits n = sum (splitDigits n)

validate :: Integer -> Bool
validate n =
    let digits  = toDigits n
        doubled = doubleEveryOther digits
        summed  = sumDigits doubled
        isValid = (summed `mod` 10) == 0
    in isValid
