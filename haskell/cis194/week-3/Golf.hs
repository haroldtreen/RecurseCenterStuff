module Golf where
    skips :: [a] -> [[a]]
    skips [] = []
    skips list = [[list !! (charPos-1) | charPos <- [offset, 2 * offset..length list], mod charPos offset == 0] | offset <- [1..length list]]

    localMaxima :: [Integer] -> [Integer]
    localMaxima [] = []
    localMaxima list = [ list !! maxima | maxima <- [1..length list - 1], isMaxima list maxima]
        where
            isMaxima l i = l !! (i - 1) < (l !! i) && l !! (i + 1) < (l !! i)
