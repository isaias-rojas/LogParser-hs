module Main(main) where

import Test.Hspec
import LogParser


main :: IO ()
main = hspec $ do
    describe "parseMessage" $ do
        it "parses info messages" $ do
            parseMessage "I 6 Completed armadillo processing" `shouldBe`
                LogMessage Info 6 "Completed armadillo processing"
            parseMessage "I 1 Nothing to report" `shouldBe`
                LogMessage Info 1 "Nothing to report"
            parseMessage "I 4 Everything normal" `shouldBe`
                LogMessage Info 4 "Everything normal"
            parseMessage "I 11 Initiating self-destruct sequence" `shouldBe`
                LogMessage Info 11 "Initiating self-destruct sequence"
            parseMessage "I 7 Out for lunch, back in two time steps" `shouldBe`
                LogMessage Info 7 "Out for lunch, back in two time steps"
            parseMessage "I 9 Back from lunch" `shouldBe`
                LogMessage Info 9 "Back from lunch"

        it "parses error messages" $ do
            parseMessage "E 70 3 Way too many pickles" `shouldBe`
                LogMessage (Error 70) 3 "Way too many pickles"
            parseMessage "E 65 8 Bad pickle-flange interaction detected" `shouldBe`
                LogMessage (Error 65) 8 "Bad pickle-flange interaction detected"
            parseMessage "E 20 2 Too many pickles" `shouldBe`
                LogMessage (Error 20) 2 "Too many pickles"
            parseMessage "E 99 10 Flange failed!" `shouldBe`
                LogMessage (Error 99) 10 "Flange failed!"

        it "parses warning messages" $ do
            parseMessage "W 5 Flange is due for a check-up" `shouldBe`
                LogMessage Warning 5 "Flange is due for a check-up"

    describe "insert" $ do
        it "ignores Unknown messages" $ do
            let tree = Node Leaf (LogMessage Info 5 "Message 5") Leaf
                unknownMsg = Unknown "Unknown message"
                updatedTree = insert unknownMsg tree
            updatedTree `shouldBe` tree
    
    describe "build" $ do
        it "builds a MessageTree from a list of LogMessages" $ do
            let messages = [
                    LogMessage Info 1 "Message 1",
                    LogMessage (Error 10) 2 "Message 2",
                    LogMessage Warning 3 "Message 3"]
            let tree = build messages
            tree `shouldBe`
                Node
                    (Node
                        (Node Leaf (LogMessage Info 1 "Message 1") Leaf)
                        (LogMessage (Error 10) 2 "Message 2")
                        Leaf)
                    (LogMessage Warning 3 "Message 3")
                    Leaf

        it "builds a MessageTree from a list with duplicate timestamps" $ do
            let messages = [
                    LogMessage Info 1 "Message 1",
                    LogMessage (Error 10) 2 "Message 2",
                    LogMessage Warning 2 "Message 3"]
            let tree = build messages
            tree `shouldBe`
                Node
                    (Node
                        (Node Leaf (LogMessage Info 1 "Message 1") Leaf)
                        (LogMessage (Error 10) 2 "Message 2")
                        Leaf)
                    (LogMessage Warning 2 "Message 3")
                    Leaf
    
    describe "inOrder" $ do
        it "returns an empty list for an empty MessageTree" $ do
            let tree = Leaf
            inOrder tree `shouldBe` []

        it "returns a sorted list of LogMessages from a MessageTree" $ do
            let tree = Node
                    (Node
                        (Node Leaf (LogMessage Info 1 "Message 1") Leaf)
                        (LogMessage (Error 10) 2 "Message 2")
                        (Node Leaf (LogMessage Warning 3 "Message 3") Leaf)
                    )
                    (LogMessage Info 6 "Message 6")
                    (Node
                        (Node Leaf (LogMessage Info 7 "Message 7") Leaf)
                        (LogMessage (Error 65) 8 "Message 8")
                        (Node
                            (Node Leaf (LogMessage Info 9 "Message 9") Leaf)
                            (LogMessage (Error 99) 10 "Message 10")
                            (Node Leaf (LogMessage Info 11 "Message 11") Leaf)
                        )
                    )
            inOrder tree `shouldBe`
                [ LogMessage Info 1 "Message 1"
                , LogMessage (Error 10) 2 "Message 2"
                , LogMessage Warning 3 "Message 3"
                , LogMessage Info 6 "Message 6"
                , LogMessage Info 7 "Message 7"
                , LogMessage (Error 65) 8 "Message 8"
                , LogMessage Info 9 "Message 9"
                , LogMessage (Error 99) 10 "Message 10"
                , LogMessage Info 11 "Message 11"]

        it "returns a sorted list of LogMessages after building a MessageTree" $ do
            let messages = [
                    LogMessage Info 6 "Message 6",
                    LogMessage Info 1 "Message 1",
                    LogMessage Info 9 "Message 9",
                    LogMessage (Error 10) 2 "Message 2",
                    LogMessage (Error 65) 8 "Message 8",
                    LogMessage Warning 3 "Message 3",
                    LogMessage Info 7 "Message 7",
                    LogMessage (Error 99) 10 "Message 10",
                    LogMessage Info 11 "Message 11"]
            let tree = build messages
            inOrder tree `shouldBe`
                [ LogMessage Info 1 "Message 1"
                , LogMessage (Error 10) 2 "Message 2"
                , LogMessage Warning 3 "Message 3"
                , LogMessage Info 6 "Message 6"
                , LogMessage Info 7 "Message 7"
                , LogMessage (Error 65) 8 "Message 8"
                , LogMessage Info 9 "Message 9"
                , LogMessage (Error 99) 10 "Message 10"
                , LogMessage Info 11 "Message 11"]
    
    describe "whatWentWrong" $ do
        it "returns an empty list for an empty list of LogMessages" $ do
            let messages = []
            whatWentWrong messages `shouldBe` []

        it "returns an empty list if no severe errors are present" $ do
            let messages = [
                    LogMessage Info 1 "Message 1",
                    LogMessage Warning 2 "Message 2",
                    LogMessage (Error 20) 3 "Message 3"]
            whatWentWrong messages `shouldBe` []

        it "returns severe error messages sorted by timestamp" $ do
            let messages = [
                    LogMessage Info 6 "Completed armadillo processing",
                    LogMessage Info 1 "Nothing to report",
                    LogMessage (Error 99) 10 "Flange failed!",
                    LogMessage Info 4 "Everything normal",
                    LogMessage Info 11 "Initiating self-destruct sequence",
                    LogMessage (Error 70) 3 "Way too many pickles",
                    LogMessage (Error 65) 8 "Bad pickle-flange interaction detected",
                    LogMessage Warning 5 "Flange is due for a check-up",
                    LogMessage Info 7 "Out for lunch, back in two time steps",
                    LogMessage (Error 20) 2 "Too many pickles",
                    LogMessage Info 9 "Back from lunch"]
            whatWentWrong messages `shouldBe`
                [ "Way too many pickles"
                , "Bad pickle-flange interaction detected"
                , "Flange failed!"]