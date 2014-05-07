----------------------------------------------------------------------
--- @author Michael Kirkedal Thomsen <kirkedal@acm.org>
--- @copyright (C) 2013, Michael Kirkedal Thomsen
--- @doc
--- Abstract syntax tree for RFun
---   T. Yokoyama, H. B. Axelsen, and R. Gluck
---   Towards a reversible functional language
---   LNCS vol. 7165, pp. 14--29, 2012
--- @end
--- Created : Dec 2013 by Michael Kirkedal Thomsen <kirkedal@acm.org>
----------------------------------------------------------------------

-- Grammar:                                         
-- q ::= d*                           (program)
-- d ::= f l 􏰟 e                     (definition)
-- l ::= x                            (variable)
--     | c(l1,...,ln)                 (constructor)
--     | ⌊l⌋                          (duplication/equality)
-- e ::= l                            (left-expression)
--     | let lout = f lin in e        (let-expression)
--     | rlet lin = f lout in e       (rlet-expression)
--     | case l of {li → ei}+         (case-expression)
-- 
-- Syntax domains: 
-- q ∈ Programs
-- d ∈ Definitions 
-- f ∈ Functions
-- l ∈ Left-expressions 
-- e ∈ Expressions
-- x ∈ Variables
-- c ∈ Constructors
--
-- Abstract syntax of the first-order functional language (n ≥ 0, m ≥ 1) The Language

module Ast where

import qualified Data.Map as M
import Data.List (intersperse)

type Program  = [Func]
data Func     = Func { funcname  :: Ident
                     , param     :: LExpr
                     , body      :: Expr}
              deriving (Eq, Show)
data Expr     = LeftE LExpr
              | LetIn LExpr Ident LExpr Expr
              | RLetIn LExpr Ident LExpr Expr
              | CaseOf LExpr [(LExpr, Expr)]
              deriving (Show, Eq)
data LExpr    = Var Ident
              | Constr Ident [LExpr]
              | DupEq LExpr
              deriving (Show, Eq)
type Ident    = String


-- A value (p. 16) is defined as
-- * a constructor of 0 or more values
-- * with two special case constructors of singleton and tuple
data Value = ConstrV Ident [Value]
           deriving (Show, Eq)

type Error = String
type Eval a = Either Error a

type FuncEnv = M.Map Ident Func

class Pretty a where
  pretty :: a -> String

instance Pretty Func where
  pretty (Func funcname param body) = funcname ++ " " ++ pretty param ++ " =^= " ++ pretty body

instance Pretty LExpr where
  pretty (Var ident) = ident
  pretty (Constr eIdent []) = eIdent
  pretty (Constr eIdent lExprs) = eIdent ++ "(" ++ (concat $ intersperse ", " $ map pretty lExprs) ++ ")"
--  pretty (UnTup lExpr) = "{" ++ pretty lExpr ++ "}"
--  pretty (BinTup lExpr1 lExpr2) = "{" ++ pretty lExpr1 ++ ", " ++ pretty lExpr2 ++ "}"
  pretty (DupEq lExpr) = "|" ++ pretty lExpr ++ "|"

instance Pretty Expr where
  pretty (LeftE lExpr) = pretty lExpr
  pretty (LetIn lExpr_out ident lExpr_in expr) =
        "let " ++ pretty lExpr_out ++ " = " ++ pretty lExpr_in ++ " in " ++ pretty expr
  pretty (RLetIn lExpr_in ident lExpr_out expr) =
        "rlet " ++ pretty lExpr_in ++ " = " ++ pretty lExpr_out ++ " in " ++ pretty expr
  pretty (CaseOf lExpr matches) =
        "case " ++ pretty lExpr ++ " of " ++ "{" ++ concatMap (\(le,e) -> pretty le ++ " -> " ++ pretty e) matches ++ "}"

instance Pretty Value where
  pretty (ConstrV "Cons" [value1,value2]) = 
	(pretty value1) ++ " : " ++ (pretty value2)
  pretty (ConstrV "Nil" []) = "[ ]"
  pretty (ConstrV ident []) = ident
  pretty (ConstrV ident values) = 
	ident ++ "(" ++ foldl1 (\x y -> x ++ ", " ++ y) vals ++ ")"
	where
		vals = map pretty values
 -- pretty (UnTupV  value) = 
	--"{" ++ pretty value ++ "}"
 -- pretty (BinTupV value1 value2) = 
	--"{" ++ pretty value1 ++ ", " ++ pretty value2 ++ "}"
