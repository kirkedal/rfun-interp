-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Michael Kirkedal Thomsen, 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen <kirkedal@acm.org>
-- Stability   :  Running
-- Portability :
--
-- |Main file for interpreting RFun
--
-- The language is based on, to which the are make references in the comments:
--
--   T. Yokoyama, H. B. Axelsen, and R. Gluck
--   Towards a reversible functional language
--   LNCS vol. 7165, pp. 14--29, 2012
--
-----------------------------------------------------------------------------

module Main (main) where

import Ast
import Parser (parseFromFile, parseValue, parseString, ParseError)
import Preparse (runPreparse)
import Interp (runProg)
import System.Environment
import System.Exit
import System.Timeout


-- |Main function
main :: IO ()
main = 
	do 
		args <- getArgs
		case args of
			[program, value, filename] -> 
				do
					res <- timeout (5 * 1000000) $ parseAndRun program value filename -- 5 second
					case res of
						Nothing -> exitWith $ ExitFailure 124
						_       -> return ()
			[filename] -> parseAndPre filename
			_ -> putStrLn "Bad args. Usage: \"main\" startfunc startvalue programfile\nor to stop before interpretation: \"main\" programfile "


parseAndRun :: String -> String -> String -> IO ()
parseAndRun program value filename =
	do
		prg    <- fromParsecError =<< parseInput filename
		val    <- fromParsecError =<< parseValue value
		let funEnv = runPreparse prg
		res    <- fromError =<< return (runProg program val funEnv)
		putStrLn $ pretty res

parseAndPre :: String -> IO ()
parseAndPre filename =
	do
		prg    <- fromParsecError =<< parseInput filename
		let funEnv = runPreparse prg
--		putStrLn $ prettyFuncEnv funEnv
		putStrLn $ show funEnv

parseInput :: String -> IO (Either ParseError Program)
parseInput "-"  = parseString =<< getContents
parseInput file = parseFromFile file

fromParsecError :: Either ParseError a -> IO a
fromParsecError (Left err) = putStr ((show err) ++ "\n") >> (exitWith $ ExitFailure 1)
fromParsecError (Right a)  = return a

fromError :: Eval a -> IO a
fromError (Left err) = putStr (err ++ "\n") >> (exitWith $ ExitFailure 1)
fromError (Right a)  = return a
