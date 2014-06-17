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
		program2 = applyToFunctionBody desugarManyLetIn program
		program3 = applyToFunctionBody desugarApplyInExpr program2
		funcEnvSS = programToFuncEnvSS program3
		funcEnv = desugarArgPatMatch funcEnvSS

applyToFunctionBody :: (Expr -> Expr) -> Program -> Program
applyToFunctionBody fun prog = map app prog
	where
		app func = func{ body = fun $ body func }

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

-------------------------------------------------------------------------------
-- ** De-sugar function calls in expressions
-------------------------------------------------------------------------------
desugarApplyInExpr :: Expr -> Expr
desugarApplyInExpr (LetIn lExpr1 ident lExpr2 expr) =
	LetIn lExpr1 ident lExpr2 $ desugarApplyInExpr expr
desugarApplyInExpr (RLetIn lExpr1 ident lExpr2 expr) =
	RLetIn lExpr1 ident lExpr2 $ desugarApplyInExpr expr
desugarApplyInExpr (CaseOf lExpr cases) =
	CaseOf lExpr $ map (\(le,e) -> (le, desugarApplyInExpr e)) cases
desugarApplyInExpr (ApplyE ident lExpr) = 
	LetIn (Var "_tmp") ident lExpr (LeftE (Var "_tmp"))
desugarApplyInExpr e = e

-------------------------------------------------------------------------------
-- ** De-sugar many let assignments
-------------------------------------------------------------------------------
desugarManyLetIn :: Expr -> Expr
desugarManyLetIn (LetIns lets expr) =
	foldr (\(lExpr1, ident, lExpr2) e -> LetIn lExpr1 ident lExpr2 e) exprDS lets
	where 
		exprDS = desugarManyLetIn expr
desugarManyLetIn (RLetIns lets expr) =
	foldr (\(lExpr1, ident, lExpr2) e -> RLetIn lExpr1 ident lExpr2 e) exprDS lets
	where 
		exprDS = desugarManyLetIn expr
desugarManyLetIn (LetIn lExpr1 ident lExpr2 expr) =
	LetIn lExpr1 ident lExpr2 $ desugarManyLetIn expr
desugarManyLetIn (RLetIn lExpr1 ident lExpr2 expr) =
	RLetIn lExpr1 ident lExpr2 $ desugarManyLetIn expr
desugarManyLetIn (CaseOf lExpr cases) =
	CaseOf lExpr $ map (\(le,e) -> (le, desugarManyLetIn e)) cases
desugarManyLetIn e = e


