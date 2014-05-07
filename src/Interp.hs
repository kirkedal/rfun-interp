----------------------------------------------------------------------
--- @author Michael Kirkedal Thomsen <kirkedal@acm.org>
--- @copyright (C) 2013, Michael Kirkedal Thomsen
--- @doc
--- Implementation an interpreter for RFun
--- The design is intended to follow closely* the design of the RFun paper:
---   T. Yokoyama, H. B. Axelsen, and R. Gluck
---   Towards a reversible functional language
---   LNCS vol. 7165, pp. 14--29, 2012
--- * I know that there are obvious reader and state monads below.
--- @end
--- Created : Dec 2013 by Michael Kirkedal Thomsen <kirkedal@acm.org>
----------------------------------------------------------------------

--module RFunInterp(runProg) where
module Interp where

import Ast
import PrettyPrint
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.List

evalMaybe :: Error -> Maybe a -> Eval a
evalMaybe e Nothing = failEval e
evalMaybe _ (Just a) = return a

failEval = Left

-------------------------------------------------------------------------------
--- Values and functions on these
-------------------------------------------------------------------------------

-- Converting a value to a left-expression
valueToLExpr :: Value -> LExpr
valueToLExpr (ConstrV ident values) = 
	Constr ident (map valueToLExpr values)

-------------------------------------------------------------------------------
--- Substitutions and functions on these
-------------------------------------------------------------------------------
-- A substitution is a mapping from a Ident (string) to a value
type Substitution = M.Map Ident Value

-- Make a substution of one variable
idSub :: Substitution
idSub = M.empty

-- Make a substution of one variable
newSub :: Ident -> Value -> Substitution
newSub ident value = M.singleton ident value

-- Lookup a value in a substitution
lookupValue :: Ident -> Substitution -> Eval Value
lookupValue ident sub 
	| M.size sub == 1  = evalMaybe ("Variable "++ ident ++" not found") $ M.lookup ident sub
	| otherwise        = failEval "Substitution is not singleton"

-- Divides a substitution into two. First part contains the mappings contained
-- in the list of Idents, the second does not. 
divide :: [Ident] -> Substitution -> Eval (Substitution, Substitution)
divide idents sub = 
	if M.size sub1 == length idents
	then return (sub1, sub2)
	else failEval $ "Variables not found when dividing"
	where
		(sub1, sub2) = M.partitionWithKey (\k _ -> elem k idents) sub

-- Lookup a value in a substitution
lookupDivide :: Ident -> Substitution -> Eval (Value, Substitution)
lookupDivide ident sub = 
	do
		(singleton, rest) <- divide [ident] sub 
		value <- lookupValue ident singleton
	 	return (value, rest)


disUnion :: Substitution -> Substitution -> Eval Substitution
disUnion subs1 subs2 = 
	if union_size == subs1_size + subs2_size
	then return union
	else failEval "Substitutions are not disjoint"
	where
		union = M.union subs1 subs2
		union_size = M.size union
		subs1_size = M.size subs1
		subs2_size = M.size subs2

-- Finds the union between two disjoint substitutions (no overlap in idents). 
disjointUnion_M :: Eval Substitution -> Eval Substitution -> Eval Substitution
disjointUnion_M subs1 subs2 = join $ liftM2 disUnion subs1 subs2

-- Finds the union between a list of disjoint substitutions.
disjointUnions_M :: [Eval Substitution] -> Eval Substitution
disjointUnions_M subs = foldl disjointUnion_M (return idSub) subs


-------------------------------------------------------------------------------
--- Program functions
-------------------------------------------------------------------------------

lookupFunction :: FuncEnv -> Ident -> Eval (LExpr, Expr)
lookupFunction funcEnv ident = 
	case M.lookup ident funcEnv of 
		Just(func) -> return (param func, body func)
		otherwise  -> failEval ("Function "++ ident ++" not found")


-------------------------------------------------------------------------------
--- The interpreter
-------------------------------------------------------------------------------

-- Eq/Dup operator: Eqs. 3 and 4, p. 17
evalDupEq :: Value -> Eval Value
-- Unary tuple is copied
evalDupEq (ConstrV "Tuple" [value]) = return $ ConstrV "Tuple" [value,value]
-- Binary tuple becomes a unary if values are equal, otherwise unchanged
evalDupEq (ConstrV "Tuple" [value1,value2])
	| value1 == value2 = return $ ConstrV "Tuple" [value1]
	| otherwise        = return $ ConstrV "Tuple" [value1, value2]
evalDupEq _ = failEval "Value is not a unary or binary tuple"

-- R-Match: Fig. 2, p. 18
-- Returns a substitution
evalRMatchS :: Value -> LExpr -> Eval Substitution
-- Single variable resulting in a signleton substitusion
evalRMatchS value (Var ident) = return $ newSub ident value
-- Constructor or a special constructor
evalRMatchS (ConstrV vIdent values) (Constr eIdent lExprs) = 
	if ((length values) == (length lExprs)) && (vIdent == eIdent)
	then disjointUnions_M $ zipWith evalRMatchS values lExprs
	else failEval $ "Different constructors " ++ show (ConstrV vIdent values) ++ " |vs| " ++ show (Constr eIdent lExprs)
-- Dublication / Equality
evalRMatchS value (DupEq lExpr) = do
	dupEq <- evalDupEq value
	evalRMatchS dupEq lExpr

-- Returns a value
evalRMatchV :: Substitution -> LExpr -> Eval Value
-- Single variable resulting in a single value
evalRMatchV sub (Var ident) = lookupValue ident sub
-- Constructor or a special constructor
evalRMatchV sub (Constr eIdent lExprs) = 
	do
		vals <- zipWithM (flip evalRMatchV) lExprs =<< subsf
		return $ ConstrV eIdent vals
 	where
		vars  = map findVars lExprs
		subs  = mapM (flip divide $ sub) vars
		subsf = liftM (map fst) subs
-- Not sure that this makes sense
evalRMatchV sub (DupEq lExpr) = evalDupEq =<< evalRMatchV sub lExpr


-- Function calls: Fig 3, p. 19, FunExp
-- Function calls in a sub part of lets
evalFunS :: FuncEnv -> Ident -> LExpr -> Value -> Eval Substitution
evalFunS funcEnv ident lExpr value =
	do
		(lExprFun, exprFun) <- lookupFunction funcEnv ident
		sub_f <- evalExpS funcEnv exprFun value
		val_p <-  evalRMatchV sub_f lExprFun
		evalExpS funcEnv (LeftE lExpr) val_p
	
evalFunV :: FuncEnv -> Substitution -> Ident -> LExpr -> Eval Value
evalFunV funcEnv sub ident lExpr =
	do
		(lExprFun, exprFun) <- lookupFunction funcEnv ident
		val_p <- evalExpV funcEnv sub (LeftE lExpr)
		sub_f <- evalRMatchS val_p lExprFun
		evalExpV funcEnv sub_f exprFun

-- Expressions: Fig 3, p. 19 (not FunExp)
evalExpS :: FuncEnv -> Expr -> Value -> Eval Substitution
evalExpS _ (LeftE lExpr) value = evalRMatchS value lExpr
evalExpS funcEnv (LetIn lExpr_out ident lExpr_in expr) value =
	do
		sub_end <- evalExpS funcEnv expr value
		(sub_out, sub_e) <- divide vars sub_end 
		val_out <- evalRMatchV sub_out lExpr_out
		sub_in <- evalFunS funcEnv ident lExpr_in val_out
		disUnion sub_in sub_e
	where
		vars = findVars lExpr_out
evalExpS funcEnv (RLetIn lExpr_in ident lExpr_out expr) value =
	do
		sub_end <- evalExpS funcEnv expr value
		(sub_out, sub_e) <- divide vars sub_end
		val_in <- evalFunV funcEnv sub_out ident lExpr_out
		sub_in <- evalRMatchS val_in lExpr_in
		disUnion sub_in sub_e
	where
		vars = findVars lExpr_out
evalExpS _ (CaseOf _ _) _ = 
	failEval "Cases that return substitutions are never used"

evalExpV :: FuncEnv -> Substitution -> Expr -> Eval Value
evalExpV _ sub (LeftE lExpr) = evalRMatchV sub lExpr
evalExpV funcEnv sub (LetIn lExpr_out ident lExpr_in expr) =
	do
		(sub_in, sub_e) <- divide vars sub
		val_out <- evalFunV funcEnv sub_in ident lExpr_in
		sub_out <- evalRMatchS val_out lExpr_out
		sub_end <- disUnion sub_out sub_e
		evalExpV funcEnv sub_end expr
	where
		vars = findVars lExpr_in
evalExpV funcEnv sub (RLetIn lExpr_in ident lExpr_out expr) =
	do
		(sub_in, sub_e) <- divide vars sub
		val_in <- evalRMatchV sub_in lExpr_in
		sub_out <- evalFunS funcEnv ident lExpr_out val_in
		sub_end <- disUnion sub_out sub_e
		evalExpV funcEnv sub_end expr
	where 
		vars = findVars lExpr_in
evalExpV funcEnv sub e@(CaseOf lExpr matches) = 
	do
		(sub_l, sub_t) <- divide vars sub
		val_p <- evalExpV funcEnv sub_l (LeftE lExpr)
		(j, sub_j) <- evalMaybe ("No match in cases: " ++ (pretty e) ++ " of value:" ++ pretty val_p) $
					 findSubIndex (evalRMatchS val_p) $ map fst matches
		sub_jt <- disUnion sub_j sub_t
		val <- evalExpV funcEnv sub_jt $ snd $ matches !! j
		takenMatches <- (\x -> return $ take x matches) j
		let takenExpr = map snd takenMatches
		    leaves_j = concatMap leaves takenExpr
		evalMaybe ("Return value match in preceding leaves: " ++ pretty val_p) $ checkLeaves evalRMatchS val leaves_j
	where 
		vars = findVars lExpr


-- This function is helper for the caseOf
checkLeaves :: (Value -> LExpr -> Eval c) -> Value -> [LExpr] -> Maybe Value
checkLeaves _ val []          = return (val)
checkLeaves func val (l:list) =
		either (\_ -> checkLeaves func val list) (\_ -> Nothing) $ func val l

-- The list is indexed from 0!!!!
-- Different from the paper
findSubIndex :: (a -> Eval b) -> [a] -> Maybe (Int, b)
findSubIndex func list =
	findSubIndex_h 0 func list
	where
		findSubIndex_h _ _ [] = Nothing
		findSubIndex_h i f (l:ls) = 
			either (\_ -> (findSubIndex_h (i+1) f ls)) (\r -> return (i,r)) (f l)

-- As defined in Footnote 1, p 19.
leaves :: Expr -> [LExpr]
leaves (LeftE lExpr)       = [lExpr]
leaves (LetIn _ _ _ expr)  = leaves expr
leaves (RLetIn _ _ _ expr) = leaves expr
leaves (CaseOf _ matches)  = concatMap (leaves . snd) matches

-- Finds the list of all variables in a left expression
findVars :: LExpr -> [Ident]
findVars (Var ident)            = [ident]
findVars (Constr _ lExprs)      = concatMap findVars lExprs
findVars (DupEq lExpr)          = findVars lExpr

-- Running a program
runProg :: Ident -> Value -> FuncEnv -> Eval Value
runProg ident value funcEnv = 
	evalFunV funcEnv idSub ident (valueToLExpr value)

