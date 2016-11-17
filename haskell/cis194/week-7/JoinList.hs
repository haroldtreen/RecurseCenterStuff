{-# LANGUAGE FlexibleInstances #-}
module JoinList where
    import Editor
    import Buffer
    import Sized
    import Scrabble

    data JoinList m a = Empty
        | Single m a
        | Append m (JoinList m a) (JoinList m a)
        deriving (Eq, Show)

    instance Buffer (JoinList (Score, Size) String) where
        toString = unlines . jlToList
        fromString = foldl (\p c -> p +++ (Single (scoreString c, 1) c)) Empty . lines
        line = indexJ
        numLines (Single (_, (Size len)) _) = len
        numLines (Append (_, (Size len)) _ _) = len
        numLines (Empty) = 0
        replaceLine num str buf = takeJ num buf +++ fromString str +++ dropJ (num + 1) buf
        value (Single (Score val, _) _) = val
        value (Append (Score val, _) _ _) = val
        value (Empty) = 0

    (+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
    (+++) list1 list2 = Append (m1 `mappend` m2) list1 list2
        where
            m1 = tag list1
            m2 = tag list2

    tag :: Monoid m => JoinList m a -> m
    tag (Single m _) = m
    tag (Append m _ _) = m
    tag (Empty) = mempty

    tagSize :: (Sized m, Monoid m) => JoinList m a -> Int
    tagSize = getSize . size . tag

    indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
    indexJ 0 (Single m a) = Just a
    indexJ index (Append m list1 list2)
        | index < listSize = indexJ index list1
        | index >= listSize = indexJ (index - listSize) list2
        | otherwise = Nothing
        where
            listSize = tagSize list1
    indexJ _ _ = Nothing

    dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
    dropJ num item@(Single m a)
        | num <= 0 = item
        | num > 0 = Empty
    dropJ num root@(Append m list1 list2)
        | num >= rootSize = Empty
        | num >= listSize = dropJ (num - listSize) list2
        | num > 0 = dropJ num list1 +++ list2
        | otherwise = root
        where
            listSize = tagSize list1
            rootSize = tagSize root
    dropJ _ _ = Empty

    takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
    takeJ num l@(Single _ _)
        | num > 0 = l
    takeJ num root@(Append m list1 list2)
        | num >= rootSize = root
        | num < listSize = takeJ num list1
        | num >= listSize = list1 +++ (takeJ (num - listSize) list2)
        where
            rootSize = (getSize . size) m
            listSize = tagSize list1
    takeJ _ _ = Empty

    jlToList :: JoinList m a -> [a]
    jlToList Empty = []
    jlToList (Single _ a) = [a]
    jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

    scoreLine :: String -> JoinList Score String
    scoreLine str = Single score str
        where
            score = scoreString str

    -- Main

    main = runEditor editor $ (fromString (unlines (map (show) [1..100])) :: (JoinList (Score, Size) String))
