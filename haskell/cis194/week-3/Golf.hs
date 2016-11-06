module Golf where
    import qualified Data.Map as M

    skips :: [a] -> [[a]]
    skips [] = []
    skips list = [[list !! (charPos-1) | charPos <- [offset, 2 * offset..length list], mod charPos offset == 0] | offset <- [1..length list]]

    localMaxima :: [Integer] -> [Integer]
    localMaxima [] = []
    localMaxima list = [ list !! maxima | maxima <- [1..length list - 1], isMaxima list maxima]
        where
            isMaxima l i = l !! (i - 1) < (l !! i) && l !! (i + 1) < (l !! i)


    histogram :: [Integer] -> String
    histogram nums = unlines [ (getRow countsMap rowNum) | rowNum <- [histogramMax, histogramMax - 1..1] ] ++ unlines [replicate 10 '=', "0123456789"]
        where
            countsMap = foldl (\m v -> M.insertWith (+) v 1 m) M.empty nums
            histogramMax = maximum $ M.elems countsMap

    getRow :: M.Map Integer Integer -> Integer -> String
    getRow counts rowNum = foldl (\str col -> if (M.findWithDefault 0 col counts >= rowNum) then do str ++ "*" else do str ++ " ") "" [0..9]
