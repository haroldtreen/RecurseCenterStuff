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
