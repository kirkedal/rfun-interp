----------------------------------------------------------------------
--- @author Michael Kirkedal Thomsen <kirkedal@acm.org>
--- @copyright (C) 2014, Michael Kirkedal Thomsen
--- @doc
--- Pretty printer for RFun
--- @end
--- Created : May 2014 by Michael Kirkedal Thomsen <kirkedal@acm.org>
----------------------------------------------------------------------

module PrettyPrint where

import Ast
import Data.List

-------------------------------------------------------------------------------
--- Very simple tabbing function
-------------------------------------------------------------------------------

doTab :: String -> String
doTab s = concatMap addtab s
	where
		addtab '\n' = ['\n','\t']
		addtab   c  = [c]

-------------------------------------------------------------------------------
--- Values to string
-------------------------------------------------------------------------------

valueToString :: Value -> String
valueToString (ConstrV "Tuple" values) = 
	"{" ++ (concat $ intersperse ", " vals) ++ "}"
	where
		vals = map valueToString values
valueToString (ConstrV "Cons" [value1,value2]) = 
	(valueToString value1) ++ " : " ++ (valueToString value2)
valueToString (ConstrV "Nil" []) = "[ ]"
valueToString (ConstrV ident []) = ident
valueToString (ConstrV ident values) = 
	ident ++ "(" ++ (concat $ intersperse ", " vals) ++ ")"
	where
		vals = map valueToString values

-------------------------------------------------------------------------------
--- Pretty print of programs
-------------------------------------------------------------------------------

ppLExpr :: LExpr -> String
ppLExpr (Var ident) = ident
ppLExpr (Constr "Tuple" lExprs) = 
	"{" ++ (concat $ intersperse ", " $ map ppLExpr lExprs) ++ "}"
ppLExpr (Constr "Cons" [lExpr1, lExpr2]) = 
	(ppLExpr lExpr1) ++ " : " ++ (ppLExpr lExpr2)
ppLExpr (Constr "Nil" []) = "[ ]"
ppLExpr (Constr ident []) = ident
ppLExpr (Constr ident lExprs) = 
	ident ++ "(" ++ (concat $ intersperse ", " $ map ppLExpr lExprs) ++ ")"
ppLExpr (DupEq lExpr) = "|" ++ ppLExpr lExpr ++ "|"

ppExpr :: Expr -> String
ppExpr (LeftE lExpr) = ppLExpr lExpr
ppExpr (LetIn lExprOut ident lExprIn expr) =
	"let " ++ ppLExpr lExprOut ++ " = " ++ ident ++ " " ++ ppLExpr lExprIn ++ " in " ++ ppExpr expr
ppExpr (RLetIn lExprIn ident lExprOut expr) = 
	"rlet " ++ ppLExpr lExprIn ++ " = " ++ ident ++ " " ++ ppLExpr lExprOut ++ " in " ++ ppExpr expr
ppExpr (CaseOf lExpr cases) =
	"case " ++ ppLExpr lExpr ++ " of" ++ (doTab $ concatMap ppCase cases)
	where
		ppCase (lE, e) = "\n" ++ ppLExpr lE ++ " -> " ++ ppExpr e

ppFunc :: Func -> String
ppFunc func =
	"\n" ++ funcname func ++ " " ++ ppLExpr (param func) ++ " =^=\n" ++ ppExpr (body func)

ppProgram :: Program -> String
ppProgram program = concat $ intersperse "\n" $ map ppFunc program
