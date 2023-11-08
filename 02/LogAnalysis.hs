{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseContent :: [String] -> (TimeStamp, String)
parseContent (s:ls) = ((read s :: TimeStamp), (unwords ls))
parseContent _ = error "invalid"

parseMessage :: String -> LogMessage
parseMessage msg = case words msg of
    ("I":left) -> uncurry (LogMessage Info) (parseContent left)
    ("W":left) -> uncurry (LogMessage Warning) (parseContent left)
    ("E":line:left) -> uncurry (LogMessage (Error (read line :: Int))) (parseContent left)
    _ -> Unknown msg

parse :: String -> [LogMessage]
parse msg = map parseMessage (lines msg)

getTimestamp :: LogMessage -> TimeStamp
getTimestamp (LogMessage _ time _) = time
getTimestamp (Unknown _) = 0

getMsg :: LogMessage -> String
getMsg (LogMessage _ _ msg) = msg
getMsg (Unknown msg) = msg

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node left root right) = case (getTimestamp msg) < (getTimestamp root) of
    True -> Node (insert msg left) root right
    False -> Node left root (insert msg right)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = (inOrder left) ++ (root:(inOrder right))

visitInOrder :: (LogMessage -> a) -> (LogMessage -> Bool) -> MessageTree -> [a]
visitInOrder f pred Leaf = []
visitInOrder f pred (Node left root right) = case pred root of
    True -> (visitInOrder f pred left) ++ ((f root):(visitInOrder f pred right))
    False -> (visitInOrder f pred left) ++ (visitInOrder f pred right)

check :: LogMessage -> Bool
check (LogMessage (Error level) _ _) = level > 50
check _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msg = visitInOrder getMsg check (build msg)


main = do
    tree <- testParse parse 11 "sample.log"
    print (inOrder (build tree))