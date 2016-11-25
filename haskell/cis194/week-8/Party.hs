{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where
    import Employee
    import Data.Tree
    import Data.List

    glCons :: Employee -> GuestList -> GuestList
    glCons emp (GL emps fun) = (GL (emps ++ [emp]) (fun + empFun emp))

    instance Monoid GuestList where
        mempty = (GL [] 0)
        mappend (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

    treeFold :: (a -> [b] -> b) -> Tree a -> b
    treeFold f t@(Node root children) = f root (map (treeFold f) children)

    moreFun :: GuestList -> GuestList -> GuestList
    moreFun list1 list2
        | list1 > list2 = list1
        | list1 <= list2 = list2

    nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
    nextLevel boss [] = (GL [boss] (empFun boss), GL [] 0)
    nextLevel boss lists = (mappend maxWOBoss bossList, maxWBoss)
        where
            maxWOBoss = foldl (mappend) (GL [] 0) (map (snd) lists)
            maxWBoss = foldl (mappend) (GL [] 0) (map (uncurry moreFun) lists)
            bossList = GL [boss] (empFun boss)

    maxFun :: Tree Employee -> GuestList
    maxFun tree = moreFun possibleList1 possibleList2
        where
            possibleLists = treeFold nextLevel tree
            possibleList1 = fst possibleLists
            possibleList2 = snd possibleLists

    guestListToString :: GuestList -> String
    guestListToString list@(GL employees fun) = title ++ names
        where
            title = "Total Fun: " ++ (show fun)
            names = foldl (\list empl -> list ++ "\n" ++ (empName empl)) "" employees

    main :: IO ()
    main = do
            companyTree <- readFile "company.txt"
            let guestlist = maxFun (read companyTree :: Tree Employee)
            putStrLn $ guestListToString guestlist
