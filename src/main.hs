import Parser
import Scanner
import TypeChecker
import Interpreter
import Compiler
import Serializer

import Control.Monad (unless)
import System.IO (hFlush, stdout)


-- TODO: make this better c:
main :: IO ()
main =  do
    prompt
    line <- getLine
    unless (null line) $ do
        let 
            ast    =  parse $ tokenize line
            tp     =  typeCheck ast
        case tp of
            Left msg -> do
                putStrLn $ "Error: " ++ msg
                main
            _ -> return ()
        print $ eval ast
        main

prompt :: IO ()
prompt = do
    putStr ">> "
    hFlush stdout