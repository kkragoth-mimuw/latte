module Main where


import           Control.Monad                 (when)
import qualified Data.Map                      as Map
import           Data.Typeable
import           System.Environment            (getArgs, getProgName)
import           System.Exit                   (exitFailure, exitSuccess)
import           System.IO                     (hGetContents, stdin)
import           System.FilePath
import           System.Process

import           AbsLatte
import           LexLatte
import           ParLatte
import           PrintLatte
import           SkelLatte


import           Typechecker (runTypecheck, pprintTypecheckerErrorMsg, initTCEnv)
import           LLVMCompiler

import           ErrM

type ParseFun = [Token] -> Err Program

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun -> FilePath -> IO ()
runFile v p f = readFile f >>= \s -> run v p s (Just f)

run :: Verbosity -> ParseFun -> String -> Maybe FilePath -> IO ()
run _ _ [] _ = exitSuccess
run v p s filePathM = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "ERROR\n"
                          putStrLn "\nParse failed...\n"
                          exitFailure
           Ok  tree -> case runTypecheck initTCEnv tree of
                            Left error -> do
                              putStrLn "ERROR\n"
                              putStrLn "typechecker error"
                              pprintTypecheckerErrorMsg error
                              exitFailure
                            Right _ -> do
                                      compilerInfo <- runLLVMCompiler tree
                                      case compilerInfo of
                                        Left error -> do
                                          putStrLn "ERROR\n"
                                          putStrLn error
                                        Right result -> do
                                          case filePathM of
                                            Nothing -> putStr (result)
                                            Just f -> do
                                              
                                              let lliFile = (dropExtension f) <.> "ll"
                                              let bcFile = (dropExtension f) <.> "bc"
                                              let bcFileTmp = (dropExtension f) <.> "bcTMP"
                                              writeFile lliFile result
                                              callCommand $ "llvm-as -o " ++ (show bcFileTmp) ++ " " ++ (show lliFile)
                                              callCommand $ "llvm-link -o " ++ (show bcFile) ++ " lib/runtime.bc " ++ (show bcFileTmp)
                                              callCommand $ "rm -f " ++ (show bcFileTmp)

                                              putStrLn "OK"
                                     


showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= (\s -> run 2 pProgram s Nothing)
    "-s":fs    -> mapM_ (runFile 0 pProgram) fs
    fs         -> mapM_ (runFile 2 pProgram) fs





