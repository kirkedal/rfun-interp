-----------------------------------------------------------------------------
--
-- Module      :  Ast
-- Copyright   :  Michael Kirkedal Thomsen, 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen <kirkedal@acm.org>
-- Stability   :  
-- Portability :
--
-- |Abstract syntax tree for RFun
--
-- The language is based on, to which the are make references in the comments:
--
-- Grammar:                                         
-- q ::= d*                           (program)
-- d ::= f l =^= e                    (definition)
-- l ::= x                            (variable)
--     | c(l_1,...,l_n)               (constructor)
--     | |l|                          (duplication/equality)
-- e ::= l                            (left-expression)
--     | let l_out = f l_in in e      (let-expression)
--     | rlet l_in = f l_out in e     (rlet-expression)
--     | case l of {l_i -> e_i}+      (case-expression)
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
--
-----------------------------------------------------------------------------

module Ast where

import qualified Data.Map as M
import Data.List (intercalate)

-- |A program is a sequence of functions
type Program  = [Func]
-- |A function is an identifier is some left-expression as parameter and
--  an expression as body
data Func     = Func { funcname  :: Ident
                     , param     :: LExpr
                     , body      :: Expr}
              deriving (Eq, Show)
-- |An expression is
data Expr     = LeftE LExpr                       -- ^ Left-expression
              | LetIn LExpr Ident LExpr Expr      -- ^ Let-binding
              | RLetIn LExpr Ident LExpr Expr     -- ^ Let-binding with reverse function call
              | CaseOf LExpr [(LExpr, Expr)]      -- ^ Case-of expression
              deriving (Show, Eq)
-- |A left-expression is
data LExpr    = Var Ident                         -- ^ Variable
              | Constr Ident [LExpr]              -- ^ Constructor term
              | DupEq LExpr                       -- ^ Duplication / equality test
              deriving (Show, Eq)
-- |Identifiers at simple Strings
type Ident    = String


-- |A value (p. 16) is defined as
-- * a constructor of 0 or more values
data Value = ConstrV Ident [Value]
           deriving (Show, Eq)

-- |Converting a value to a left-expression
valueToLExpr :: Value -> LExpr
valueToLExpr (ConstrV ident values) = 
  Constr ident (map valueToLExpr values)

-- |An error is a String
type Error = String
-- |Evaluating with return either a result of an Error
type Eval a = Either Error a

-- |Function environments (to be used later) is a mapping from Identifiers to a Function
type FuncEnv = M.Map Ident Func

-- |Pretty for showing programs and values
class Pretty a where
  pretty :: a -> String

instance Pretty Func where
  pretty (Func funcname param body) = funcname ++ " " ++ pretty param ++ " =^= " ++ pretty body

instance Pretty LExpr where
  pretty (Var ident) = ident
  pretty (Constr "Cons" [lExpr1,lExpr2]) = (pretty lExpr1) ++ " : " ++ (pretty lExpr2)
  pretty (Constr "Nil" []) = "[ ]"
  pretty (Constr "Tuple" lExprs) = "{" ++ (intercalate ", " $ map pretty lExprs) ++ "}"
  pretty (Constr eIdent []) = eIdent
  pretty (Constr eIdent lExprs) = eIdent ++ "(" ++ (intercalate ", " $ map pretty lExprs) ++ ")"
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
  pretty value = pretty $ valueToLExpr value
