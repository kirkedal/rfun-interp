


module Interp (interp) where

import Ast
import qualified Core.Ast as C
import Core.Preparse (runPreparse)
import Core.Interp (runProg)

import Data.Maybe (catMaybes)


    -- res    <- fromError $ runProg program val funEnv

-- fromError :: Either ErrorRFun (Value, EvalTrace) -> IO Value
-- fromError (Left err) = putStr (err ++ "\n") >> (exitWith $ ExitFailure 1)
-- fromError (Right a)  = return $ fst a


interp :: Program -> String -> [Value] -> Either String Value
interp p i vs =
  -- Left $ C.prettyFuncEnv funEnv
  case runProg i (tcValues vs) funEnv of
    (Left err) -> Left err
    (Right a)  -> fromCoreValue $ fst a
  where
    funEnv = runPreparse $ toCore p

fromCoreValue :: C.Value -> Either String Value
fromCoreValue (C.ConstrV "Tuple" vs) = Right $ fcValue $ last vs
fromCoreValue v = Left $ show $ fcValue v

toCore :: Program -> C.Program
toCore p = (C.Func "id" (C.Var "_ctmp") (C.LeftE (C.Var "_ctmp"))):
           (C.Func "eq" (C.Var "_ctmp")
             (C.CaseOf (C.DupEq (C.Var "_ctmp"))
               [(C.Constr "Tuple" [C.Var "_ctmp"], C.LeftE (C.Constr "Tuple" [C.Var "_ctmp", C.Constr "Tuple" []]))])):
           (catMaybes $ map tcFunc p)

tcFunc :: Func -> Maybe C.Func
tcFunc (Func ident _ clauses) = Just $ C.Func (tcIdent ident) (C.Var "_ctmp") (C.CaseOf (C.Var "_ctmp") (map tcClause clauses))
tcFunc _ = Nothing

tcClause :: Clause -> (C.LExpr, C.Expr)
tcClause (Clause _ params _ expr) = ((C.Constr "Tuple" lExprs), e)
  where
    lExprs = map tcLExpr params
    e = tcExpr (init lExprs) expr

tcExpr :: [C.LExpr] -> Expr -> C.Expr
tcExpr params (LeftE (App ident True lExprs)) =
    C.LetIn (C.Constr "Tuple" ((init les) ++ [C.Var "_ctmp"])) (tcIdent ident) (C.Constr "Tuple" les)
      (C.LeftE $ C.Constr "Tuple" (params ++ [C.Var "_ctmp"]))
    where
      les = map tcLExpr lExprs
tcExpr params (LeftE (App ident False lExprs)) =
    C.RLetIn (C.Constr "Tuple" les) (tcIdent ident) (C.Constr "Tuple" ((init les) ++ [C.Var "_ctmp"]))
      (C.LeftE $ C.Constr "Tuple" (params ++ [C.Var "_ctmp"]))
    where
      les = map tcLExpr lExprs
tcExpr params (LeftE lExpr) = C.LeftE $ C.Constr "Tuple" (params ++ [tcLExpr lExpr])
tcExpr params (CaseOf lExpr cases) = C.CaseOf (tcLExpr lExpr) (map (\(le,_,e) -> (tcLExpr le, tcExpr params e)) cases)
tcExpr params (LetIn leftLE (App ident True funLEs) expr) =  -- Forward application
    C.LetIn (C.Constr "Tuple" leftLEs) (tcIdent ident) (C.Constr "Tuple" rightLEs) (tcExpr params expr)
  where
    rightLEs = map tcLExpr funLEs
    leftLEs  = (init rightLEs) ++ [tcLExpr leftLE]
tcExpr params (LetIn leftLE (App ident False funLEs) expr) =  -- Backward application
    C.RLetIn (C.Constr "Tuple" leftLEs) (tcIdent ident) (C.Constr "Tuple" rightLEs) (tcExpr params expr)
  where
    leftLEs = map tcLExpr funLEs
    rightLEs  = (init leftLEs) ++ [tcLExpr leftLE]
tcExpr params (LetIn leftLE rightLE expr) =  C.LetIn (tcLExpr leftLE) "id" (tcLExpr rightLE) (tcExpr params expr) -- No function application

tcLExpr :: LExpr -> C.LExpr
tcLExpr (Var ident) = C.Var $ tcIdent ident
tcLExpr (Constr ident lExprs) = C.Constr (tcIdent ident) (map tcLExpr lExprs)
tcLExpr (Int    integer) = constToConstr integer
tcLExpr (Tuple  lExprs) = C.Constr "Tuple" (map tcLExpr lExprs)
tcLExpr (List   (ListCons lExpr lExprList)) = C.Constr "Cons" [tcLExpr lExpr, tcLExpr (List lExprList)]
tcLExpr (List   (ListEnd  lExpr)) = tcLExpr lExpr
tcLExpr (List   ListNil) = C.Constr "Nil" []
-- tcLExpr (App ident True lExprs) = 

tcIdent :: Ident -> C.Ident
tcIdent (Ident s _) = s

tcValues :: [Value] -> C.Value
tcValues vs = C.ConstrV "Tuple" (map tcValue vs)

tcValue :: Value -> C.Value
tcValue (IntV integer)  = constToValue integer
tcValue (TupleV values) = C.ConstrV "Tuple" (map tcValue values)
tcValue (ListV  [])     = C.ConstrV "Nil" []
tcValue (ListV  (v:vs)) = C.ConstrV "Cons" [tcValue v, tcValue (ListV vs)]
tcValue (ConstrV ident values) = C.ConstrV ident (map tcValue values)
tcValue (FunV i) = C.ConstrV i []

fcValue :: C.Value -> Value
fcValue (C.ConstrV "Z" [])  = IntV 0
fcValue (C.ConstrV "S" [c]) =
  case fcValue c of
    IntV i -> IntV $ i + 1
    v -> ConstrV "S" [v]
fcValue (C.ConstrV "P" [c]) =
  case fcValue c of
    IntV i -> IntV $ i - 1
    v -> ConstrV "P" [v]
fcValue (C.ConstrV "Tuple" vs) = TupleV $ map fcValue vs
fcValue (C.ConstrV "Nil" []) = ListV []
fcValue (C.ConstrV "Cons" [v1,v2]) =
  case fcValue v2 of
    ListV t -> ListV $ (fcValue v1):t
    v -> ConstrV "Cons" [fcValue v1,v]
fcValue (C.ConstrV c vs) = ConstrV c $ map fcValue vs

constToValue :: Integer -> C.Value
constToValue n
    | n == 0    = C.ConstrV "Z" []
    | n <  0    = C.ConstrV "P" [constToValue (n+1)]
    | otherwise = C.ConstrV "S" [constToValue (n-1)]

constToConstr :: Integer -> C.LExpr
constToConstr n
    | n == 0    = C.Constr "Z" []
    | n <  0    = C.Constr "P" [constToConstr (n+1)]
    | otherwise = C.Constr "S" [constToConstr (n-1)]
