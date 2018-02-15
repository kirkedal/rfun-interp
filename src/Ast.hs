-----------------------------------------------------------------------------
--
-- Module      :  Ast
-- Copyright   :  Michael Kirkedal Thomsen, 2017
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen <kirkedal@acm.org>
-- Stability   :  None guaranteed?
-- Portability :
--
-- |Abstract syntax tree for RFun17
--
-- The language is based on, to which the are make references in the comments:
--
--
-----------------------------------------------------------------------------

module Ast where

import qualified Text.Megaparsec as P  -- (SourcePos, SourcePos(..), Pos(..))
import qualified Data.Map as M

import Data.Complex

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

data BType    = IntT | QbitT | DataT Ident | ListT BType | ProdT [BType] | SumT [BType] | FunT TypeSig | VarT Ident | AnyT
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
                      , sourcePos  :: P.SourcePos }
              deriving (Eq, Show)

-- |A value (p. 16) is defined as
-- * a constructor of 0 or more values
data Value = IntV Integer
           | QbitV (Complex Double)
           | TupleV [Value]
           | ListV  [Value]
           | ConstrV String [Value]
           | FunV String
           deriving (Show, Eq)



type FuncEnv = M.Map String Func

makeFunEnv :: Program -> FuncEnv
makeFunEnv p = M.fromList $ map (\x -> ((identifier. funcName) x, x)) p

makeIdent :: String -> Ident
makeIdent s = Ident s (P.SourcePos "" (P.unsafePos 1) (P.unsafePos 1))