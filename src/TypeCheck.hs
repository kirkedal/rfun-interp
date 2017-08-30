---------------------------------------------------------------------------
--
-- Module      :  TypeCheck
-- Copyright   :  Michael Kirkedal Thomsen, 2017
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen <kirkedal@acm.org>
-- Stability   :  none?
-- Portability :  ?
--
-- |Simple type check for RFun17
--
-----------------------------------------------------------------------------

module TypeCheck (typecheck) where

import Ast
import PrettyPrinter
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

-- import Data.List (intersperse)

typecheck :: Program -> Maybe String
typecheck p = catchTCError $ hts >> cfd >> ltc
  where
    hts = mapError hasTypeSignature p
    cfd = mapError checkFunctionDefinitions p
    ltc = mapError (checkFunc (fenvFromProgram p)) p
 -- Get function names and definitions
 -- Check each function
 -- Check first-match policy
 -- Check that value had correct type

type TCError a = Either String a

noTypeError :: TCError ()
noTypeError = return ()

catchTCError :: TCError () -> Maybe String
catchTCError (Right _) = Nothing
catchTCError (Left l ) = return l

mapError :: (a -> TCError ()) -> [a] -> TCError ()
mapError f l =
  case (mapM f l) of
    (Right _) -> return ()
    (Left e ) -> fail e

maybeError :: Maybe a -> a
maybeError Nothing  = error "Cannot be nothing"
maybeError (Just x) = x


-- Check names

-- |Check Function types and definitions
checkFunctionDefinitions :: Func -> TCError ()
checkFunctionDefinitions func@(Func _ _ _) =
  mapError checkFunctionClause $ funcClause func
  where
    checkFunctionClause clause | length (clauseParam clause) /= length typeDefList = fail $ errorDifferentNumberArgs (clauseIdent clause) (funcName func)
    checkFunctionClause clause = mapError (\(x,y) -> checkTypeMatchLExpr (clauseIdent clause) x y) (zip typeDefList (clauseParam clause))
    typeDefList = typeDefList_ $ funcTypesig func
    typeDefList_ Nothing = []
    typeDefList_ (Just (TypeSig ancT leftT _)) = ancT ++ [leftT]
checkFunctionDefinitions (DataType _ _) = noTypeError
-- TypeSig [BType] BType

-- |Check if all functions have type signatures
hasTypeSignature :: Func -> TCError ()
hasTypeSignature (Func i _ _) | (identifier i) == "eq" = fail $ "eq is a reserved function name."
hasTypeSignature (Func i _ _) | (identifier i) == "id" = fail $ "id is a reserved function name."
hasTypeSignature func@(Func _ _ _) =
  case (funcTypesig func) of
    (Just _) -> noTypeError
    Nothing  -> fail $ errorNoTypeSignature (funcName func)
hasTypeSignature (DataType i _) | (identifier i) == "EQ" = fail $ "EQ is a reserved datatype name."
hasTypeSignature (DataType _ _) = noTypeError


---------

checkTypeMatchLExpr :: Ident -> BType -> LExpr -> TCError ()
checkTypeMatchLExpr i t le =
  case getLExprType le of
    Nothing  -> fail $ errorTypeMatch i t le
    (Just leType) ->
      case bTypeUnifies t leType of
        True  -> noTypeError
        False -> fail $ errorTypeMatch i t le

typeEquality :: TypeSig -> TypeSig -> Bool
typeEquality (TypeSig ancT1 leftT1 rightT1) (TypeSig ancT2 leftT2 rightT2) | length ancT1 == length ancT2 =
  and $ zipWith bTypeUnifies (leftT1:rightT1:ancT1) (leftT2:rightT2:ancT2)
typeEquality _ _ = False

bTypeUnifies :: BType -> BType -> Bool
-- bTypeUnifies NatT NatT = True
bTypeUnifies (DataT i1) (DataT i2) | identifier i1 == identifier i2 = True
bTypeUnifies (ListT t1) (ListT t2) = bTypeUnifies t1 t2
bTypeUnifies (ProdT t1) (ProdT t2) | length t1 == length t2 = and $ zipWith bTypeUnifies t1 t2
bTypeUnifies (SumT  t1) (SumT  t2) | length t1 == length t2 = and $ zipWith bTypeUnifies t1 t2
bTypeUnifies (FunT  t1) (FunT  t2) = typeEquality t1 t2
bTypeUnifies (VarT  i1) (VarT  i2) | identifier i1 == identifier i2 = True
bTypeUnifies  AnyT       _         = True
bTypeUnifies  _          AnyT      = True
bTypeUnifies  _          _         = False



typeUnification :: TypeSig -> TypeSig -> Maybe TypeSig
typeUnification (TypeSig ancTs1 leftT1 rightT1) (TypeSig ancTs2 leftT2 rightT2) =
  do ancT   <- sequence $ zipWith bTypeUnification ancTs1 ancTs2
     leftT  <- bTypeUnification leftT1 leftT2
     rightT <- bTypeUnification rightT1 rightT2
     return $ TypeSig ancT leftT rightT

bTypeUnification :: BType -> BType -> Maybe BType
bTypeUnification t@(DataT i1) (DataT i2) | identifier i1 == identifier i2 = Just t
bTypeUnification (ListT t1) (ListT t2) =
  case bTypeUnification t1 t2 of
    Nothing  -> Nothing
    (Just t) -> Just $ ListT t
bTypeUnification (ProdT t1) (ProdT t2) | length t1 == length t2 =
  case sequence $ zipWith bTypeUnification t1 t2 of
    Nothing  -> Nothing
    (Just t) -> Just $ ProdT t
-- bTypeUnification (SumT  t1) (SumT  t2) | length t1 == length t2 = and $ zipWith bTypeUnification t1 t2
bTypeUnification (FunT  t1) (FunT  t2) =
  case typeUnification t1 t2 of
    Nothing  -> Nothing
    (Just t) -> Just $ FunT t
bTypeUnification t@(VarT _) (VarT _) = Just t
-- bTypeUnification t@(VarT i1) (VarT i2) | identifier i1 == identifier i2 = Just t
bTypeUnification  AnyT       t         = Just t
bTypeUnification  t          AnyT      = Just t
bTypeUnification  _          _         = Nothing

getLExprType :: LExpr -> Maybe BType
getLExprType (Var _) = Just AnyT -- Variable can be any type
getLExprType (Int _) = Just $ DataT $ makeIdent "Nat"
-- getLExprType (Constr i []) | (identifier i == "Z") = Just NatT
-- getLExprType (Constr i [lExpr]) | (identifier i == "S") = (getLExprType lExpr) >>= (bTypeUnification NatT)
getLExprType (Constr _ _) = Just AnyT -- I need function Env
getLExprType (Tuple  lExprs) = (sequence $ map getLExprType lExprs) >>= (\x -> Just $ ProdT x)
              -- DataT Ident -- ^ Constructor term
getLExprType (List lExprList) = getListLExprType lExprList >>= (\t -> return $ ListT t)
  where
    getListLExprType (ListCons lExpr lExprL) =
      do t1 <- getLExprType lExpr
         t2 <- getListLExprType lExprL
         bTypeUnification t1 t2
    getListLExprType (ListEnd lExpr) = getLExprType lExpr
    getListLExprType (ListNil) = Just AnyT
getLExprType (App _ _ _) = Just AnyT

-- data BType    = NatT | AnyT Ident | ListT BType | ProdT [BType] | SumT [BType] | FunT TypeSig
--               deriving (Eq, Show)

-- Check Linearity
-- Check Ancillae

type FunEnv = M.Map String Func

fenvFromProgram :: Program -> FunEnv
fenvFromProgram p = M.fromList $ (eqTD:(map f p))
  where f func@(Func _ _ _) = ((identifier.funcName) func, func)
        f dataT@(DataType _ _) = ((identifier.dataName) dataT, dataT)
        eqTD = ("EQ", DataType (makeIdent "EQ") (M.fromList [
                  ("Eq",  (makeIdent "Eq", [])),
                  ("Neq", (makeIdent "Neq", [AnyT]))]) )

data VarType = Ancillae BType | Live BType | Killed
             deriving (Eq, Show)

type Vars = M.Map String VarType


newtype TC a = E { runE :: StateT Vars (ReaderT FunEnv (Except String)) a }
               deriving (Applicative, Functor, Monad, MonadReader FunEnv, MonadState Vars, MonadError String)

runTC :: TC a -> Vars -> FunEnv -> (TCError (a, Vars))
runTC eval vars fenv = runExcept $ runReaderT (runStateT (runE eval) vars) fenv

addLive :: Ident -> BType -> TC BType
addLive i btype =
  do b <- varExist i
     when b $ throwError $ errorAddExistingVariable i  --- Can check if it is alive of dead
     modify (\x -> M.insert (identifier i) (Live btype) x)
     return btype

addAncillae :: Ident -> BType -> TC BType
addAncillae i btype =
  do b <- varExist i
     when b $ throwError $ errorAddExistingVariable i  --- Can check if it is alive of dead
     modify (\x -> M.insert (identifier i) (Ancillae btype) x)
     return btype

killLive :: Ident -> BType -> TC BType
killLive i btype =
  do c <- get
     case M.lookup (identifier i) c of
       Nothing  -> throwError $ errorUseOfNonExistingVariable i
       (Just Killed) -> throwError $ errorUseKilledVariable i
       (Just (Ancillae _)) -> throwError $ errorUseAncillaVariable i
       (Just (Live t)) ->
         case bTypeUnification btype t of
           Nothing  -> throwError $ errorDifferentTypes i t btype
           (Just ut) -> return ut

checkAncillae :: Ident -> BType -> TC BType
checkAncillae i btype =
  do c <- get
     case M.lookup (identifier i) c of
       Nothing  -> throwError $ errorUseOfNonExistingVariable i
       (Just Killed) -> throwError $ errorUseKilledVariable i
       (Just (Ancillae t)) ->
         case bTypeUnification btype t of
           Nothing  -> throwError $ errorDifferentTypes i t btype
           (Just ut) -> return ut
       (Just (Live t)) ->
         case bTypeUnification btype t of
           Nothing  -> throwError $ errorDifferentTypes i t btype
           (Just ut) -> return ut

varExist :: Ident -> TC Bool
varExist i =
  do v <- get
     return $ M.member (identifier i) v

funTypeSig :: Ident -> TC TypeSig
funTypeSig i | (identifier i) == "eq" = return $ TypeSig [VarT i] (VarT i) (DataT $ makeIdent "EQ")
funTypeSig i =
  do fenv <- ask
     case M.lookup (identifier i) fenv of
       Nothing ->
         do v <- get
            case M.lookup (identifier i) v of
              Nothing -> throwError $ errorUseOfNonExistingFunction i
              Just (Ancillae (FunT sig)) -> return sig
              _ -> throwError $ errorUseOfNonExistingFunction i
       Just (Func _ s _) -> return $ maybeError s
       _ -> throwError $ errorUseOfNonExistingFunction i

dataTypeDef :: Ident -> Ident -> TC [BType]
dataTypeDef i c =
  do fenv <- ask
     case M.lookup (identifier i) fenv of
       Nothing -> throwError $ errorUseOfNonExistingTypeDefinition i
       Just (DataType _ s) ->
        case M.lookup (identifier c) s of
          Nothing -> throwError $ errorUseOfNonExistingDataConstructor c i
          Just td -> return $ snd td
       _ -> throwError $ errorUseOfNonExistingDataConstructor c i

checkFunc :: FunEnv -> Func -> TCError ()
checkFunc fe f@(Func _ _ _)   = mapError (\x -> checkClause x (maybeError $ funcTypesig f) fe) $ funcClause f
checkFunc _ (DataType _ _) = noTypeError


-- We ignore Guards at the moment
checkClause :: Clause -> TypeSig -> FunEnv -> TCError ()
checkClause c (TypeSig ancT inT outT) fe =
  case runTC (eval (clauseParam c) (ancT ++ [inT])) (M.empty) fe of
    Left e -> fail e
    Right _ -> noTypeError
  where
    eval [x] [y] = (checkLExpr addLive x y) >> (checkExpr (clauseBody c) outT)
    eval (x:xs) (y:ys) = (checkLExpr addAncillae x y) >> (eval xs ys)
    eval _ _ = error "...."

checkExpr :: Expr -> BType -> TC ()
checkExpr (LeftE lExpr) btype = checkLExpr killLive lExpr btype >> return ()
checkExpr (LetIn leftLE rightLE expr) btype =
  do t <- checkLExpr killLive rightLE AnyT
     checkLExpr addLive leftLE t
     checkExpr expr btype
checkExpr (CaseOf lExpr cases) btype = -- [(LExpr, Guard, Expr)]      -- ^ Case-of expression
  do t <- checkLExpr killLive lExpr AnyT
     mapM_ (testCase t) cases
  where
    testCase bt (lE, _, ex) =
      do v <- get
         checkLExpr addLive lE bt
         checkExpr ex btype
         put v


checkLExpr :: (Ident -> BType -> TC BType) -> LExpr -> BType -> TC BType
checkLExpr addFun (Var ident) btype = addFun ident btype  -- Variable can be any type
-- Integers
checkLExpr _ (Int _) btype | bTypeUnifies btype (DataT $ makeIdent "Nat") = return $ DataT $ makeIdent "Nat"
checkLExpr _ lExpr@(Int _) t  = throwError $ errorLExprUnification lExpr t
-- checkLExpr _ (Constr i []) btype | (identifier i == "Z"), bTypeUnifies btype NatT = return NatT
-- checkLExpr addFun (Constr i [lExpr]) btype | (identifier i == "S") = checkLExpr addFun lExpr btype
checkLExpr addFun lExpr@(Constr i lExprs) t@(DataT typeName) =
  do dd <- dataTypeDef typeName i
     when ((length dd) /= length lExprs) $ throwError $ errorLExprUnification lExpr t
     sequence $ zipWith (checkLExpr addFun) (lExprs) dd
     return $ DataT typeName

checkLExpr addFun (Tuple  lExprs) (ProdT btypes) | length lExprs == length btypes =
  do types <- sequence $ zipWith (checkLExpr addFun) lExprs btypes
     return $ ProdT types
checkLExpr _ lExpr@(Tuple  _) t  = throwError $ errorLExprUnification lExpr t
checkLExpr addFun le@(List lExprList) tp@(ListT btype) = getListLExprType lExprList
  where
    getListLExprType (ListCons lExpr lExprL) =
      do t1 <- checkLExpr addFun lExpr btype
         t2 <- getListLExprType lExprL
         case bTypeUnification (ListT t1) t2 of
           Nothing -> throwError $ errorLExprUnification le tp
           Just t -> return t
    getListLExprType (ListEnd  lExpr) = checkLExpr addFun lExpr (ListT btype)
    getListLExprType ListNil = return tp
checkLExpr _ lExpr@(List _) t = throwError $ errorLExprUnification lExpr t
checkLExpr addFun (App ident True lExprs) _ =
  do (TypeSig ancTs updT retT) <- funTypeSig ident
     when ((length ancTs) + 1 /= length lExprs) $ throwError $ errorDifferentNumberArgsApp ident (TypeSig ancTs updT retT) lExprs
     sequence $ zipWith (checkLExpr checkAncillae) (init lExprs) ancTs
     checkLExpr addFun (last lExprs) updT
     return retT
checkLExpr addFun (App ident False lExprs) _ =
  do (TypeSig ancTs updT retT) <- funTypeSig ident
     when ((length ancTs) + 1 /= length lExprs) $ throwError $ errorDifferentNumberArgsApp ident (TypeSig ancTs updT retT) lExprs
     sequence $ zipWith (checkLExpr checkAncillae) (init lExprs) ancTs
     checkLExpr addFun (last lExprs) retT
     return updT
checkLExpr _ lExpr t = throwError $ errorLExprUnification lExpr t




errorFirst :: Ident -> String
errorFirst i_def =
  "In " ++ ppIdentFile i_def ++ ", " ++ ppIdentPos i_def ++ "\n  "

errorUseKilledVariable :: Ident -> String
errorUseKilledVariable i =
  errorFirst i ++ "the variable " ++ ppIdent i ++ " which is trying to be has already been used."

errorUseAncillaVariable :: Ident -> String
errorUseAncillaVariable i =
  errorFirst i ++ "the variable " ++ ppIdent i ++ " which is trying to be has ancillae type."

errorAddExistingVariable :: Ident -> String
errorAddExistingVariable i =
  errorFirst i ++ "the variable " ++ ppIdent i ++ " has already been defined."

errorUseOfNonExistingVariable :: Ident -> String
errorUseOfNonExistingVariable i =
  errorFirst i ++ "the variable " ++ ppIdent i ++ " is undefined."

errorUseOfNonExistingFunction :: Ident -> String
errorUseOfNonExistingFunction i =
  errorFirst i ++ "the function " ++ ppIdent i ++ " is undefined."

errorUseOfNonExistingDataConstructor :: Ident -> Ident -> String
errorUseOfNonExistingDataConstructor i t =
  errorFirst i ++ "the constructor " ++ ppIdent i ++ " in type definition " ++ ppIdent t ++ " is undefined."

errorUseOfNonExistingTypeDefinition :: Ident -> String
errorUseOfNonExistingTypeDefinition i =
  errorFirst i ++ "the type definition " ++ ppIdent i ++ " is undefined."

errorLExprUnification :: LExpr -> BType -> String
errorLExprUnification le a_type =
  "The left-expression\n  " ++ ppLExpr le ++ "\ncannot be unified with type\n  " ++ ppBType a_type ++ "\n"


errorDifferentTypes :: Ident -> BType -> BType -> String
errorDifferentTypes i_def i_type a_type =
  errorFirst i_def ++ "the variable " ++ ppIdent i_def ++ " of type\n    " ++ ppBType i_type ++ "\n" ++
  "does not have expected type\n    " ++ ppBType a_type

errorDifferentNumberArgsApp :: Ident -> TypeSig -> [LExpr] -> String
errorDifferentNumberArgsApp i_def i_sig args =
  errorFirst i_def ++ "the function \n    " ++ ppIdent i_def ++ " :: " ++ ppTypeSig i_sig ++ "\n" ++
  "is provided with " ++ (show $ length args) ++ " arguments.\n"

errorTypeMatch :: Ident -> BType -> LExpr -> String
errorTypeMatch i_def btype lExpr =
  case getLExprType lExpr of
    Nothing  -> "errorTypeMatch"
    (Just t) ->
      "In " ++ ppIdentFile i_def ++ " function " ++ ppIdent i_def ++ " (" ++ ppIdentLine i_def ++ ") " ++
      "the type of left-expression \n  " ++ ppLExpr lExpr ++ "\nof type\n  " ++ (ppBType t) ++
      "\ndoes not match type signature \n  " ++ ppBType btype ++ "\n"

errorDifferentNumberArgs :: Ident -> Ident -> String
errorDifferentNumberArgs i_def i_sig =
  "In " ++ ppIdentFile i_def ++ " function " ++ ppIdent i_def ++ " (" ++ ppIdentLine i_def ++
  ") has different number of arguments than in type signature (" ++ ppIdentLine i_sig ++ ").\n"

errorNoTypeSignature :: Ident -> String
errorNoTypeSignature i =
  "In " ++ ppIdentFile i ++ " function " ++ ppIdent i ++ " (" ++ ppIdentPos i ++ ") has not type signature.\n" ++
  "  Type inference is not supported yet."
