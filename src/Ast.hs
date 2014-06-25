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

constrToNum :: LExpr -> Maybe Int
constrToNum (Constr "Z" []) = Just 0
constrToNum (Constr "S" [lExpr]) = do n <- constrToNum lExpr ; Just $ n + 1
constrToNum _c = Nothing

-- |An error is a String
type Error = String
-- |Evaluating with return either a result of an Error
type Eval a = Either Error a

-- |Function environments (to be used later) is a mapping from Identifiers to a Function
type FuncEnv = M.Map Ident Func

prettyFuncEnv :: FuncEnv -> String
prettyFuncEnv funcEnv = 
  intercalate "\n" $ map (pretty.snd) $ M.toList funcEnv

-- |Pretty for showing programs and values
class Pretty a where
  pretty :: a -> String

instance Pretty Func where
  pretty (Func funname funparam funbody) = funname ++ " " ++ pretty funparam ++ " =^= \n" ++ pretty funbody

instance Pretty LExpr where
  pretty (Var ident) = ident
  pretty (Constr "Z" []) = "0"
  pretty c@(Constr "S" lExprs) = 
    case constrToNum c of
      Just n -> show n
      Nothing -> "S(" ++ (intercalate ", " $ map pretty lExprs) ++ ")"
  pretty (Constr "Cons" [lExpr1,lExpr2]) = 
    case getList lExpr2 of
      Just(val) -> "[" ++ (intercalate ", " $ map pretty (lExpr1 : val)) ++ "]"
      Nothing -> "(" ++ pretty lExpr1 ++ " : " ++ pretty lExpr2 ++ ")"
  pretty (Constr "Nil" []) = "[ ]"
  pretty (Constr "Tuple" lExprs) = "{" ++ (intercalate ", " $ map pretty lExprs) ++ "}"
  pretty (Constr eIdent []) = eIdent
  pretty (Constr eIdent lExprs) = eIdent ++ "(" ++ (intercalate ", " $ map pretty lExprs) ++ ")"
  pretty (DupEq lExpr) = "|" ++ pretty lExpr ++ "|"

instance Pretty Expr where
  pretty (LeftE lExpr) = pretty lExpr
  pretty (LetIn lExpr_out ident lExpr_in expr) =
        "let " ++ pretty lExpr_out ++ " = " ++ ident ++ " " ++ pretty lExpr_in ++ "\n in " ++ pretty expr ++ "\n"
  pretty (RLetIn lExpr_in ident lExpr_out expr) =
        "rlet " ++ pretty lExpr_in ++ " = " ++ ident ++ " " ++ pretty lExpr_out ++ "\n in " ++ pretty expr ++ "\n"
  pretty (CaseOf lExpr matches) =
        "case " ++ pretty lExpr ++ " of " ++ "{\n" ++ intercalate "\n" (map (\(le,e) -> pretty le ++ " -> " ++ pretty e) matches) ++ "}"

getList :: LExpr -> Maybe [LExpr]
getList (Constr "Nil" []) = Just([])
getList (Constr "Cons" [lExpr1,lExpr2]) = 
    do v <- getList lExpr2
       return $ lExpr1 : v
getList _ = Nothing

instance Pretty Value where
  pretty value = pretty $ valueToLExpr value
