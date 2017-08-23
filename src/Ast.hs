-----------------------------------------------------------------------------
--
-- Module      :  Ast
-- Copyright   :  Michael Kirkedal Thomsen, 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen <kirkedal@acm.org>
-- Stability   :  None guaranteed?
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

-- import qualified Data.Map as M
import Text.Megaparsec (SourcePos)
import qualified Data.Map as M

-- data Module   = Module { moduleName    :: Ident
--                        , moduleExports :: [Ident]
--                        , moduleImports :: [Ident]
--                        , moduleProgram :: Program}
--               deriving (Eq, Show)
-- -- |A program is a sequence of functions

type Program  = [Func]

-- |A function is an identifier is some left-expression as parameter and
--  an expression as body
data Func     = Func { funcName     :: Ident
                     , funcTypesig  :: Maybe TypeSig
                     , funcClause   :: [Clause] }
              | DataType { dataName :: Ident
                         , dataDef  :: M.Map String (Ident, [BType]) }
              deriving (Eq, Show)

data Clause   = Clause { clauseIdent :: Ident
                       , clauseParam :: [LExpr]
                       , clauseGuard :: Guard
                       , clauseBody  :: Expr }
              deriving (Eq, Show)

data TypeSig  = TypeSig [BType] BType BType
              deriving (Eq, Show)

data BType    = NatT | DataT Ident | ListT BType | ProdT [BType] | SumT [BType] | FunT TypeSig | VarT Ident | AnyT
              deriving (Eq, Show)

-- |An expression is
data Expr     = LeftE LExpr                       -- ^ Left-expression
              | LetIn LExpr LExpr Expr      -- ^ Let-binding
              | CaseOf LExpr [(LExpr, Guard, Expr)]      -- ^ Case-of expression
              deriving (Show, Eq)

data Guard = Guard [LExpr]
           deriving (Eq, Show)

-- |A left-expression is
data LExpr    = Var Ident                         -- ^ Variable
              | Constr Ident [LExpr]              -- ^ Constructor term
              | Int    Integer
              | Tuple  [LExpr]              -- ^ Constructor term
              | List   LExprList
              | App Ident Bool [LExpr]
              deriving (Show, Eq)

data LExprList = ListCons LExpr LExprList
               | ListEnd  LExpr
               | ListNil
              deriving (Show, Eq)

-- |Identifiers at simple Strings
data Ident    = Ident { identifier :: String
                      , sourcePos  :: SourcePos }
              deriving (Eq, Show)

-- |A value (p. 16) is defined as
-- * a constructor of 0 or more values
data Value = IntV Integer
           | TupleV [Value]
           | ListV  [Value]
           | ConstrV String [Value]
           | FunV String
           deriving (Show, Eq)




type FuncEnv = M.Map String Func

makeFunEnv :: Program -> FuncEnv
makeFunEnv p = M.fromList $ map (\x -> ((identifier. funcName) x, x)) p
