-----------------------------------------------------------------------------
--
-- Module      :  RFun2Prog
-- Copyright   :  Michael Kirkedal Thomsen, 2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen <kirkedal@acm.org>
-- Stability   :  None guaranteed?
-- Portability :
--
-- |Parses a RFun program to an input program for the self-interpreter.
--
-----------------------------------------------------------------------------

module Core.RFun2Prog (parseRFun) where

import Core.Ast
import Control.Monad.State
import qualified Data.Map as M
import Data.List (intercalate, sortBy)

parseRFun :: Ident -> Program -> String
parseRFun program funcs =  evalState run (1, M.empty)
  where 
    run = 
      do 
        s <- getFreshId program Function
        p <- parseProgram funcs
        g <- get
        let h = sortBy (\(_,x) (_,y) -> compare x y) $ M.toList $ snd g
        return $ "{" ++ show s ++ "," ++ stringList2 p ++ "}" ++ "\n\n" ++ show h

  
parseProgram :: Program -> IntNames [String]
parseProgram funcs = 
  do
    list <- mapM parseFunc funcs
    let slist = sortBy (\(x,_) (y,_) -> compare x y) list
        sslist = myscan 0 slist
    return $ map (\(x,y) -> "{" ++ show x ++ "," ++ y ++ "}") sslist
  where
    myscan _ [] = []
    myscan a ((x,y):xs) = (x-a,y):(myscan x xs)

parseFunc :: Func -> IntNames (Int,String)
parseFunc func = 
  do
    fId <- getFreshId (funcname func) Function
    pId <- parseLExpr $ param func
    bod <- parseExpr $ body func
    return $ (fId, "{"++ pId ++ "," ++ bod ++ "}")

parseExpr :: Expr -> IntNames String
parseExpr (LeftE lExpr) = 
  do 
    lexp <- parseLExpr lExpr
    return $ "LeftE(" ++ lexp ++ ")"
parseExpr (LetIn lExpr1 ident lExpr2 expr) =
  do 
    fId  <- getFreshId ident Function
    lex1 <- parseLExpr lExpr1
    lex2 <- parseLExpr lExpr2
    ex   <- parseExpr expr
    return $ "LetIn(" ++ lex1 ++ "," ++ show fId ++ "," ++ lex2 ++ "," ++ ex ++ ")"
parseExpr (RLetIn lExpr1 ident lExpr2 expr) =
  do 
    fId  <- getFreshId ident Function
    lex1 <- parseLExpr lExpr1
    lex2 <- parseLExpr lExpr2
    ex   <- parseExpr expr
    return $ "RLetIn(" ++ lex1 ++ "," ++ show fId ++ "," ++ lex2 ++ "," ++ ex ++ ")"
parseExpr (CaseOf lExpr cases) =
  do
    lexp <- parseLExpr lExpr
    cas  <- mapM (\(x,y) -> do {l <- parseLExpr x; e <- parseExpr y; return $ "{" ++ l ++ "," ++ e ++ "}"}) cases
    return $ "CaseOf(" ++ lexp ++ "," ++ stringList cas ++ ")"

--              | CaseOf LExpr [(LExpr, Expr)]      -- ^ Case-of expression

parseLExpr :: LExpr -> IntNames String
parseLExpr (Var ident) = 
  do 
    vId <- getFreshId ident Variable
    return $ "Var(" ++ show vId ++ ")"
parseLExpr (Constr ident lExprs) =
  do 
    lexps <- mapM parseLExpr lExprs
    return $ "Constr(" ++ ident ++ "," ++ stringList lexps ++ ")"
parseLExpr (DupEq lExpr) = 
  do
    lexp <- parseLExpr lExpr
    return $ "DupEq(" ++ lexp ++ ")"

stringList :: [String] -> String
stringList list = "[" ++ intercalate "," list ++ "]"

stringList2 :: [String] -> String
stringList2 list = "[" ++ intercalate ",\n" list ++ "]"

-- Contains the next var name, a mapping
data Type = Function | Variable
  deriving (Show, Eq, Ord)
type Subst = M.Map (Ident,Type) Int
type IntNames a = State (Int, Subst) a

-- |Returns the fresh name of a given identifier
getFreshId :: Ident -> Type -> IntNames Int
getFreshId ident typ = 
  do  (nextId, subs) <- get
      let (curId, updates) = update nextId subs
      put updates
      return $ curId
  where
    update nID s = 
      case M.lookup (ident, typ) s of
        Just(val) -> (val, (nID, s))
        Nothing  -> (nID, (nID + 1, M.insert (ident, typ) nID s))

