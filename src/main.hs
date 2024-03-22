import Parser
import Scanner
import TypeChecker
import Interpreter

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
            result =  eval ast
        putStrLn $ "Type check OK! (" ++ show tp ++ ")"
        print result
        main

prompt :: IO ()
prompt = do
    putStr ">> "
    hFlush stdout