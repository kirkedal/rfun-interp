----------------------------------------------------------------------
--- @author Michael Kirkedal Thomsen <kirkedal@acm.org>
--- @copyright (C) 2014, Michael Kirkedal Thomsen
--- @doc
--- Pre-parser / de-sugaring for the interpreter of RFun
--- @end
--- Created : Apr 2014 by Michael Kirkedal Thomsen <kirkedal@acm.org>
----------------------------------------------------------------------

--module RFunInterp(runProg) where
module Preparse(runPreparse) where

import Ast
import qualified Data.Map as M
import Data.List (nub)

-------------------------------------------------------------------------------
--- Collecting functions of identical name
-------------------------------------------------------------------------------

-- Generates a list of unique function names
functionList :: Program -> [Ident]
functionList program = nub $ map funcname program

-- Generates function environment from a list of functions
programToFuncEnvSS :: Program -> [(Ident,[Func])]
programToFuncEnvSS program = map (\x -> (x, filter (\y -> funcname y == x) program)) funlist 
	where
		funlist = functionList program

-------------------------------------------------------------------------------
--- De-sugar pattern matching in arguments, while preserving order
-------------------------------------------------------------------------------

desugarArgPatMatch :: [(Ident,[Func])] -> [(Ident,Func)]
desugarArgPatMatch = map desugarArgPatMatchSingle

desugarArgPatMatchSingle :: (Ident,[Func]) -> (Ident,Func)
desugarArgPatMatchSingle (_  , []) = error "Function list cannot be empty"
desugarArgPatMatchSingle (idt, [func]) = (idt, func)
desugarArgPatMatchSingle (idt, funcs) = (idt, Func idt (Var "x") (CaseOf (Var "x") cases))
	where
		cases = map (\x -> (param x, body x)) funcs

-------------------------------------------------------------------------------
--- Pre-parse / de-sugar call function
-------------------------------------------------------------------------------

runPreparse :: Program -> FuncEnv
runPreparse program = M.fromList funcEnv
	where
		funcEnvSS = programToFuncEnvSS program
		funcEnv = desugarArgPatMatch funcEnvSS

