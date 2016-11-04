module Golf where
    skips :: [a] -> [[a]]
    skips [] = []
    skips list = [[list !! (charPos-1) | charPos <- [offset, 2 * offset..length list], mod charPos offset == 0] | offset <- [1..length list]]
