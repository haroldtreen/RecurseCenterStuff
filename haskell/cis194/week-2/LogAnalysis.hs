{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseError :: [String] -> LogMessage
parseError (errStr:timeStr:messageStrs) = LogMessage (Error errCode) timeCode message
    where
        errCode = read errStr
        timeCode = read timeStr
        message = unwords messageStrs
parseError _ = Unknown "Unknown Message"

parseNormal :: MessageType -> [String] -> LogMessage
parseNormal msgType (timeStr:messageStrs) = LogMessage msgType timeCode message
    where
        timeCode = read timeStr
        message = unwords messageStrs
parseNormal _ _ = Unknown "Unknown Message"


parseMessage :: String -> LogMessage
parseMessage (msgType:' ':msgRest)
    | msgType == 'I' = (parseNormal Info (words msgRest))
    | msgType == 'W' = (parseNormal Warning (words msgRest))
    | msgType == 'E' = (parseError (words msgRest))
    | otherwise = Unknown "Unknown Message"
parseMessage _ = Unknown "Unknown Message"

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert (Unknown _) tree = tree
insert msg (Node left rootMsg right)
    | (time msg) < (time rootMsg) = Node (insert msg left) rootMsg right
    | (time msg) > (time rootMsg) = Node left rootMsg (insert msg right)
    where
        time (LogMessage _ tm _) = tm
        time (Unknown _) = 0
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x $ build xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ [ msg ] ++ (inOrder right)

interestingError :: LogMessage -> Bool
interestingError (LogMessage (Error level) _ _)
    | level > 50 = True
    | otherwise = False
interestingError _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (msgs) = map logToString $ inOrder $ build $ filter interestingError msgs
    where
        logToString (LogMessage _ _ str) = str
        logToString (Unknown _) = ""

