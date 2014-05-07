-----------------------------------------------------------------------------
--
-- Module      :  Preparse
-- Copyright   :  Michael Kirkedal Thomsen, 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen <kirkedal@acm.org>
-- Stability   :  
-- Portability :
--
-- |Preparse for rFun language
--
-- Current purpose of this is to generate the function environment and 
-- de-sugar the function level pattern matching to case expressions.
--
-----------------------------------------------------------------------------

module Preparse where

import Ast
import qualified Data.Map as M
import Data.List (nub)

-------------------------------------------------------------------------------
-- * Pre-parse / de-sugar call function
-------------------------------------------------------------------------------

-- |Pre-parse / de-sugar call function
runPreparse :: Program -> FuncEnv
runPreparse program = M.fromList funcEnv
	where
		funcEnvSS = programToFuncEnvSS program
		funcEnv = desugarArgPatMatch funcEnvSS

-------------------------------------------------------------------------------
-- ** Collecting functions of identical name for the function environment
-------------------------------------------------------------------------------

-- |Generates a list of unique function names
functionList :: Program -> [Ident]
functionList program = nub $ map funcname program

-- |Generates function environment from a list of functions
programToFuncEnvSS :: Program -> [(Ident,[Func])]
programToFuncEnvSS program = map (\x -> (x, filter (\y -> funcname y == x) program)) funlist 
	where
		funlist = functionList program

-------------------------------------------------------------------------------
-- ** De-sugar pattern matching in arguments, while preserving order
-------------------------------------------------------------------------------

-- |De-sugar pattern matching in arguments, while preserving order
desugarArgPatMatch :: [(Ident,[Func])] -> [(Ident,Func)]
desugarArgPatMatch = map desugarArgPatMatchSingle
	where
		desugarArgPatMatchSingle (_  , []) = error "Function list cannot be empty"
		desugarArgPatMatchSingle (idt, [func]) = (idt, func)
		desugarArgPatMatchSingle (idt, funcs) = (idt, Func idt (Var "x") (CaseOf (Var "x") cases))
			where
				cases = map (\x -> (param x, body x)) funcs
