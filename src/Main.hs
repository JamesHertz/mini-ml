module Main (main) where

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
        ("editor":_) -> editorMode
        (flag:_) | flag `elem` ["-h", "--help"] -> usage
        (filename:_) -> compileFile filename

editorMode :: IO ()
editorMode = 
    handle editorHandler $ do
        putStrLn "------------"
        txt <- getContents
        case interpretProgram txt of
            Left err -> putStrLn $ formatErr txt err
            Right value -> do 
                value' <- value -- FIXME: decide what to do with this c:
                putStrLn $ " : " ++ show value' -- this will only happen for REPL
    where 
        editorHandler :: IOError -> IO ()
        editorHandler _ = editorMode


compileFile :: String -> IO ()
compileFile filename =
    handle readHandler $ do
        putStrLn $ "Compiling " ++ filename
        contents <- readFile filename 
        case compileProgram contents of
            Left err -> putStrLn $ formatErr contents err
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
                Left err -> putStrLn $ formatErr line err
                Right value -> do 
                    value' <- value -- FIXME: decide what to do with this c:
                    putStrLn $ " : " ++ show value' -- this will only happen for REPL
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
