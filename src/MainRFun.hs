module Main (main) where

import Parser
import Ast
import PrettyPrinter
import TypeCheck
-- import Interp

import System.Environment
import System.Exit

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      [str] -> parseProgram str >>= typecheckProgram >>= prettyPrintProgram
      _ -> putStrLn "Wrong number of arguments.\nUsage:\n  \"rfun\" startfunc startvalue programfile\nor to stop before interpretation:\n  \"rfun\" programfile "

-- runProgram :: Program -> 

typecheckProgram :: Program -> IO Program
typecheckProgram p =
  case typecheck p of
        Nothing  -> return p
        (Just e) -> putStrLn e >> (exitWith $ ExitFailure 1)

prettyPrintProgram :: Program -> IO ()
prettyPrintProgram = putStrLn.ppProgram

parseProgram :: String -> IO Program
parseProgram filename = parseFromFile filename >>= fromParserError

fromParserError :: Either ParserError a -> IO a
fromParserError (Left err) = (putStr (prettyParseError err)) >> (exitWith $ ExitFailure 1)
fromParserError (Right a)  = return a

