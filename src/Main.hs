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
import Preparse (runPreparse)
import Interp (runProg)
import Parser (parseFromFile, parseValue, parseString, ParseError)
import System.Environment
import System.Exit
import System.Timeout
import Control.Monad
import Control.Exception (try, evaluate, catch)

import Data.List

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
			_ -> putStrLn "Bad args. Usage: \"main\" startfunc startvalue programfile"


parseAndRun :: String -> String -> String -> IO ()
parseAndRun program value filename =
	do
		prg    <- fromError =<< parseInput filename
		val    <- fromError =<< parseValue value
		let funEnv = runPreparse prg
		res    <- fromError =<< return (runProg program val funEnv)
		putStrLn $ pretty res

parseInput :: String -> IO (Either ParseError Program)
parseInput "-"  = parseString =<< getContents
parseInput file = parseFromFile file

fromError :: Show a => Either a b -> IO b
fromError (Left err) = putStrLn (show err) >> (exitWith $ ExitFailure 1)
fromError (Right a)  = return a
