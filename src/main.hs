import Serializer
import Core
import Errors


import Control.Monad (unless)
import System.IO (hFlush, stdout, hGetContents, openTempFile, hPutStrLn, stderr, print, hPutStr, hClose)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.Directory (removeFile)
import System.Process (rawSystem)
import Control.Exception (try, handle)
import System.IO.Error (isDoesNotExistError)

-- TODO: make this better c:
main :: IO ()
main =  do
    args <- getArgs
    case args of
        [] -> putStrLn "Running the interpreter" >> runInterpreter
        (flag:_) | flag `elem` ["-h", "--help"] -> usage
        (filename:_) -> compileFile filename


compileFile :: String -> IO ()
compileFile filename =
    handle readHandler $ do
        putStrLn $ "Compiling " ++ filename
        contents <- readFile filename 
        case compileProgram contents of
            Left Error { message } -> putStrLn $ "Error: " ++ message
            Right program -> do
                    (name, handler) <- openTempFile "/tmp" "tmp.jasm"
                    hPutStr handler $ serialize program
                    hFlush handler
                    hClose handler

                    rawSystem "java" ["-jar", "bin/jasmin.jar", name]

                    -- removeFile name
                    return ()
    where 
        readHandler :: IOError -> IO a
        readHandler e 
            |  isDoesNotExistError e = printError $ "File '" ++ filename ++ "' not file."
            |  otherwise = printError $ "Unexpected error: " ++ show e


runInterpreter :: IO ()
runInterpreter = do
    prompt
    txt <- getContents
    mapM_ interpret $ lines txt
    where
        interpret line = do
            case interpretProgram line of
                Left Error { message } -> putStrLn $ "Error: " ++ message
                Right value -> print value
            prompt

-- FIXME: later c:
printError :: String -> IO a
printError msg = do
    hPutStrLn stderr msg
    exitFailure

usage :: IO a
usage = do
    name <- getProgName
    hPutStrLn stderr $ "Usage: " ++ name ++ " [-h|--help] FILE\n"
    hPutStrLn stderr "\
\If a file is provided it will compile it and produce a jvm execultable file. But if none\ 
\ is provided it will just run the interpreter REPL."
    exitFailure

prompt :: IO ()
prompt = do
    putStr ">> "
    hFlush stdout
