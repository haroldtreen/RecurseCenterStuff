module HigerOrder where
    -- Exercise 1

    fun1 :: [Integer] -> Integer
    fun1 [] = 1
    fun1 (x:xs)
        | even x = (x - 2) * fun1 xs
        | otherwise = fun1 xs

    fun1' :: [Integer] -> Integer
    fun1' = product . map (subtract 2) . filter (even)

    fun2 :: Integer -> Integer
    fun2 1 = 0
    fun2 n | even n = n + fun2 (n `div` 2)
           | otherwise = fun2 (3 * n + 1)

    fun2' :: Integer -> Integer
    fun2' = sum
        . filter even
        . takeWhile (1 /=)
        . iterate (\x -> if even x then do (x `div` 2) else do (3 * x + 1))

    -- Exercise 2

    data Tree a = Leaf
        | Node Integer (Tree a) a (Tree a)
        deriving (Show, Eq)


    foldTree :: [a] -> Tree a
    foldTree [] = Leaf
    foldTree x = foldr (\v t -> insertBalanced t v) Leaf x

    insertBalanced :: Tree a -> a -> Tree a
    insertBalanced Leaf el = Node 1 Leaf el Leaf
    insertBalanced tree@(Node height left root right) el
        | nodeHeight left < nodeHeight right = Node newLHeight newLeft root right
        | otherwise = Node newRHeight left root newRight
        where
            nodeHeight Leaf = 0
            nodeHeight (Node h _ _ _) = h
            newLHeight = newLeft `seq` (1 + (maximum [getHeight newLeft, nodeHeight right]))
            newRHeight = newRight `seq` (1 + (maximum [nodeHeight left, getHeight newRight]))
            newRight = (insertBalanced right el)
            newLeft = (insertBalanced left el)

    getHeight :: Tree a -> Integer
    getHeight Leaf = 0
    getHeight (Node _ left _ right) = 1 + (maximum [getHeight left, getHeight right])

    -- Excercise 3

    xor :: [Bool] -> Bool
    xor = (foldr (\curr cumm -> ((not cumm) && curr) || (cumm && (not curr))) False)

    map' :: (a -> b) -> [a] -> [b]
    map' trans arr = foldr (\curr cumm -> (trans curr) : cumm) [] arr

    -- Excercise 4

    sieveSundaram :: Integer -> [Integer]
    sieveSundaram n = [ 2 * i + 1 | i <- [1..n], not (i `elem` exclude)]
        where
            exclude = sieveExclude n

    sieveExclude :: Integer -> [Integer]
    sieveExclude n = filter (<= n) allNumbers
        where
            allNumbers = [ j + i + 2 * j * i | j <- [1..n], i <- [j..n] ]
