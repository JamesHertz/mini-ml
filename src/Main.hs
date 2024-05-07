module Main (main) where

import Serializer
import Core
import Errors

import Control.Monad (unless, foldM_, void)
import System.IO (hFlush, stdout, hGetContents, openTempFile, hPutStrLn, stderr, print, hPutStr, hClose, putStrLn)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.Directory (removeFile)
import System.Process (rawSystem)
import Control.Exception (try, handle)
import System.IO.Error (isDoesNotExistError)
import Language.Preprocessor.Cpphs (filename)

-- constants
lineNumberSize :: Int
lineNumberSize = 4


-- TODO: make this better c:
main :: IO ()
main =  do
    args <- getArgs
    case args of
        [] -> usage
        (flag:_) | flag `elem` ["-h", "--help"] -> usage
        ("repl": _ ) -> putStrLn "Running the interpreter REPL" >> runInterpreter
        ("editor":_) -> editorMode
        ("-c":filename:_) -> compileFile  filename
        ("-i":filename:_) -> interpretFile filename

-- TODO: make it so that editor mode also compile to JVM
editorMode :: IO ()
editorMode = 
    handle editorHandler $ do
        putStrLn "Editor on, please just write some code and use CTRL+D to signal that you have finished.\n"
        printLineNr 1
        txt <- getContents
        foldM_ (\nr _ -> do 
                printLineNr nr
                return $ nr + 1
            ) 2 $ lines txt

        putStrLn "\n"
        case interpretProgram txt of
            Left err    -> putStrLn $ formatErr txt err
            Right value -> do 
                value' <- value
                putStrLn $ " : " ++ show value' -- this will only happen for REPL
    where 
        editorHandler :: IOError -> IO ()
        editorHandler _ = editorMode

        printLineNr :: Int -> IO ()
        printLineNr nr  = do 
            let 
                lineNr = show nr
                rem    = lineNumberSize - length lineNr
                str    = if rem <= 0 then lineNr
                         else replicate rem ' ' ++ lineNr
            putStr $ str ++ " | "
            hFlush stdout


interpretFile :: String -> IO ()
interpretFile filename =
    handle readHandler $ do
        contents <- readFile filename 
        case interpretProgram contents of
            Left err     -> putStrLn $ formatErr contents err
            Right result -> void result
    where 
        readHandler :: IOError -> IO a
        readHandler e 
            |  isDoesNotExistError e = printError $ "File '" ++ filename ++ "' not file."
            |  otherwise = printError $ "Unexpected error: " ++ show e

compileFile :: String -> IO ()
compileFile filename =
    handle readHandler $ do
        putStrLn $ "Compiling " ++ filename
        contents <- readFile filename 
        case compileProgram contents of
            Left err -> putStrLn $ formatErr contents err
            Right program -> do
                    mapM_ (\s -> do 
                        putStrLn "--------\n" 
                        putStrLn (content s)
                     ) $ serialize program
                        
                    -- (name, handler) <- openTempFile "./bin" "tmp.jasm"
                    -- hPutStr handler $ serialize program
                    -- hFlush handler
                    -- hClose handler
                    --
                    -- rawSystem "java" ["-jar", "bin/jasmin.jar", name]

                    -- removeFile name
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
        prompt = putStr ">> " >> hFlush stdout
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
    -- TODO: 
    --       * use a better error message
    --       * add flags -i and -c to decide between compiling and interpreting
    hPutStrLn stderr $ "Usage: " ++ name ++ " [-h|--help] <command> \n"
    hPutStrLn stderr "\
\Where <command> can be:                                            \n\
\      repl          to run a REPL interpreter                      \n\
\      editor        to run a simple editor mode with lines numbers \n\
\      -i | -c FILE  to interpret or respectively compile (to JVM) FILE"
    exitFailure

