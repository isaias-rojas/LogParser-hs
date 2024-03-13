module LogParser 
        ( MessageType(..)
        , LogMessage(..)
        , MessageTree(..)
        , parseMessage
        , parse
        , insert
        , build
        , inOrder
        , whatWentWrong
        ) where

data MessageType = Info
                | Warning
                | Error Int
  deriving (Show, Eq)

type TimeStamp = Int
data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf | Node MessageTree LogMessage MessageTree deriving (Show, Eq)

{-- Exercise 1
  The first step is figuring out how to parse an individual message. Define a function
p a r s e M e s s a g e : : String −> LogMessage
which parses an individual line from the log file.
--}

parseMessage:: String -> LogMessage
parseMessage str = case words str of
    ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
    ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
    ("E":sev:ts:msg) -> LogMessage (Error (read sev)) (read ts) (unwords msg)
    _ -> Unknown str

parse :: String -> [LogMessage]
parse = map parseMessage . lines

{-- Exercise 2
    insert a new LogMessage into an existing MessageTree, producing a new MessageTree.
--}

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert _ (Node _ (Unknown _) _) = error "Cannot insert into a tree with Unknown messages"
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node left node@(LogMessage _ nts _) right)
    | ts <= nts = Node (insert msg left) node right
    | otherwise = Node left node (insert msg right)

{-- Exercise 3
    Once we can insert a single LogMessage into a MessageTree, we can build a complete MessageTree from a list of messages. 
    Specifically, define a function build :: [LogMessage] -> MessageTree that builds up a MessageTree containing the messages in 
    the list, by successively inserting the messages into a MessageTree (beginning with a Leaf).
--}
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

{-- Exercise 4
    Finally, define the function inOrder :: MessageTree -> [LogMessage] which takes a sorted MessageTree and produces a list of all 
    the LogMessages it contains, sorted by timestamp from smallest to biggest. (This is known as an in-order traversal of the tree.)
--}
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

{-- Exercise 5
    Now that we can build a MessageTree from a list of messages, define the function whatWentWrong :: [LogMessage] -> [String] 
    that takes an unsorted list of LogMessages, and returns a list of the messages corresponding to any errors with a 
    severity of 50 or greater, sorted by timestamp. 
    (Of course, you can use your functions from the previous exercises to do the sorting.)
--}
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages =
    map getMessage $ inOrder $ build $ filter isSevereError messages
    where
        isSevereError (LogMessage (Error severity) _ _) = severity >= 50
        isSevereError _ = False
        getMessage (LogMessage _ _ msg) = msg