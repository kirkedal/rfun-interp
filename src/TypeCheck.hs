module TypeCheck (typecheck) where

import Ast
import PrettyPrinter
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except hiding (catchError)
import Data.Functor.Identity

-- import Data.List (intersperse)

typecheck :: Program -> Maybe String
typecheck p = catchError $ hts >> cfd >> ltc
  where
    hts = mapError hasTypeSignature p
    cfd = mapError checkFunctionDefinitions p
    ltc = mapError (checkFunc (fenvFromProgram p)) p
 -- Get function names and definitions
 -- Check each function

-- | 

type TCError a = Either String a

noTypeError :: TCError ()
noTypeError = return ()

catchError :: TCError () -> Maybe String
catchError (Right _) = Nothing
catchError (Left l ) = return l

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
    checkFunctionClause clause | length (clauseParam clause) /= length typeDefList = errorDifferentNumberArgs (clauseIdent clause) (funcName func)
    checkFunctionClause clause = mapError (\(x,y) -> checkTypeMatchLExpr (clauseIdent clause) x y) (zip typeDefList (clauseParam clause))
    typeDefList = typeDefList_ $ funcTypesig func
    typeDefList_ Nothing = []
    typeDefList_ (Just (TypeSig ancT leftT _)) = ancT ++ [leftT]
checkFunctionDefinitions dataT@(DataType _ _) = noTypeError
-- TypeSig [BType] BType

-- |Check if all functions have type signatures
hasTypeSignature :: Func -> TCError ()
hasTypeSignature func@(Func i _ _) | (identifier i) == "eq" = fail $ "eq is a reserved function name."
hasTypeSignature func@(Func i _ _) | (identifier i) == "id" = fail $ "id is a reserved function name."
hasTypeSignature func@(Func _ _ _) =
  case (funcTypesig func) of
    (Just _) -> noTypeError
    Nothing  -> errorNoTypeSignature (funcName func)
hasTypeSignature dataT@(DataType _ _) = noTypeError




---------

checkTypeMatchLExpr :: Ident -> BType -> LExpr -> TCError ()
checkTypeMatchLExpr i t le =
  case getLExprType le of
    Nothing  -> errorTypeMatch i t le
    (Just leType) ->
      case bTypeUnifies t leType of
        True  -> noTypeError
        False -> errorTypeMatch i t le

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
-- bTypeUnification NatT NatT = Just NatT
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
bTypeUnification t@(VarT i1) (VarT i2) = Just t
-- bTypeUnification t@(VarT i1) (VarT i2) | identifier i1 == identifier i2 = Just t
bTypeUnification  AnyT       t         = Just t
bTypeUnification  t          AnyT      = Just t
bTypeUnification  _          _         = Nothing

getLExprType :: LExpr -> Maybe BType
getLExprType (Var _) = Just AnyT -- Variable can be any type
getLExprType (Int _) = Just NatT
-- getLExprType (Constr i []) | (identifier i == "Z") = Just NatT
-- getLExprType (Constr i [lExpr]) | (identifier i == "S") = (getLExprType lExpr) >>= (bTypeUnification NatT)
getLExprType l@(Constr i lExprs) = Just AnyT
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
fenvFromProgram p = M.fromList $ map f p
  where f func@(Func _ _ _) = ((identifier.funcName) func, func)
        f dataT@(DataType _ _) = ((identifier.dataName) dataT, dataT)

data VarType = Ancillae BType | Live BType | Killed
             deriving (Eq, Show)

type Vars = M.Map String VarType


newtype TC a = E { runE :: StateT Vars (ReaderT FunEnv (Except String)) a }
               deriving (Applicative, Functor, Monad, MonadReader FunEnv, MonadState Vars, MonadError String)

-- runTC :: TC a -> Vars -> FunEnv -> IO (a, Vars)
-- runTC :: TC (TCError ()) -> Vars -> FunEnv -> (TCError (), Vars)
runTC :: TC a -> Vars -> FunEnv -> (TCError (a, Vars))
runTC eval vars fenv = runIdentity $ runExceptT $ runReaderT (runStateT (runE eval) vars) fenv

addLive :: Ident -> BType -> TC BType
addLive i btype =
  do b <- varExist i
     when b $ throwError ".al.1.." -- If ident exist it is a type error  --- Can check if it is alive of dead
     modify (\x -> M.insert (identifier i) (Live btype) x)
     return btype

addAncillae :: Ident -> BType -> TC BType
addAncillae i btype =
  do b <- varExist i
     when b $ throwError ".aa.1.." -- If ident exist it is a type error  --- Can check if it is alive of dead
     modify (\x -> M.insert (identifier i) (Ancillae btype) x)
     return btype

killLive :: Ident -> BType -> TC BType
killLive i btype =
  do c <- get
     case M.lookup (identifier i) c of
       Nothing  -> throwError ".kl.1.." -- If does not exist it must be a wrong variable name
       (Just Killed) -> throwError ".kl.2.." -- Lookup of killed
       (Just (Ancillae _)) -> throwError ".kl.3.."
       (Just (Live t)) -> 
         case bTypeUnification btype t of
           Nothing  -> throwError $ ".kl.4.." ++ (show i) ++ " sdff+" ++ (show t)++ " sdff+" ++ (show btype) -- Type mismatch
           (Just ut) -> return ut

checkAncillae :: Ident -> BType -> TC BType
checkAncillae i btype =
  do c <- get
     case M.lookup (identifier i) c of
       Nothing  -> throwError ".ca.1.." -- If does not exist it must be a wrong variable name
       (Just Killed) -> throwError ".ca.2.." -- Lookup of killed
       (Just (Ancillae t)) ->
         case bTypeUnification btype t of
           Nothing  -> throwError $ ".ca.3.."  ++ (show i) ++ " sdff+" ++ (show t) ++ " sdff+" ++ (show btype) -- Type mismatch -- Type mismatch
           (Just ut) -> return ut
       (Just (Live t)) ->
         case bTypeUnification btype t of
           Nothing  -> throwError ".ca.3.." -- Type mismatch
           (Just ut) -> return ut

varExist :: Ident -> TC Bool
varExist i =
  do v <- get
     return $ M.member (identifier i) v

funTypeSig :: Ident -> TC TypeSig
funTypeSig i | (identifier i) == "eq" = return $ TypeSig [VarT i] (VarT i) (ProdT [])
funTypeSig i =
  do fenv <- ask
     case M.lookup (identifier i) fenv of
       Nothing ->
         do v <- get
            case M.lookup (identifier i) v of
              Nothing -> throwError ".fts.1."
              Just (Ancillae (FunT sig)) -> return sig
              _ -> throwError ".fts.2."
       Just (Func _ s _) -> return $ maybeError s
       _ -> throwError ".fts.3."

dataTypeDef :: Ident -> Ident -> TC [BType]
dataTypeDef i c =
  do fenv <- ask
     case M.lookup (identifier i) fenv of
       Nothing -> throwError ".dtd.1."
       Just (DataType _ s) ->
        case M.lookup (identifier c) s of
          Nothing -> throwError ".dtd.2."
          Just td -> return $ snd td
       _ -> throwError ".dtd.3."

checkFunc :: FunEnv -> Func -> TCError ()
checkFunc fe f@(Func _ _ _)   = mapError (\x -> checkClause x (maybeError $ funcTypesig f) fe) $ funcClause f
checkFunc fe d@(DataType _ _) = noTypeError


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
checkLExpr _ (Int _) btype | bTypeUnifies btype NatT = return NatT
checkLExpr _ (Int _) _  = throwError "type.1"
-- checkLExpr _ (Constr i []) btype | (identifier i == "Z"), bTypeUnifies btype NatT = return NatT
-- checkLExpr addFun (Constr i [lExpr]) btype | (identifier i == "S") = checkLExpr addFun lExpr btype
checkLExpr addFun (Constr i lExprs) (DataT typeName) =
  do dd <- dataTypeDef typeName i
     when ((length dd) /= length lExprs) $ throwError "differnet length"
     sequence $ zipWith (checkLExpr addFun) (lExprs) dd
     return $ DataT typeName

checkLExpr addFun (Tuple  lExprs) (ProdT btypes) | length lExprs == length btypes =
  do types <- sequence $ zipWith (checkLExpr addFun) lExprs btypes
     return $ ProdT types
checkLExpr _ (Tuple  _) _  = throwError "type.2"
checkLExpr addFun (List lExprList) (ListT btype) = getListLExprType lExprList
  where
    getListLExprType (ListCons lExpr lExprL) =
      do t1 <- checkLExpr addFun lExpr btype
         t2 <- getListLExprType lExprL
         case bTypeUnification (ListT t1) t2 of
           Nothing -> throwError "type.3."
           Just t -> return t
    getListLExprType (ListEnd  lExpr) = checkLExpr addFun lExpr (ListT btype)
    getListLExprType ListNil = return btype
checkLExpr _ (List _) _ = throwError "type.4."
checkLExpr addFun (App ident True lExprs) _ =
  do (TypeSig ancTs updT retT) <- funTypeSig ident
     when ((length ancTs) + 1 /= length lExprs) $ throwError "differnet length"
     sequence $ zipWith (checkLExpr checkAncillae) (init lExprs) ancTs
     checkLExpr addFun (last lExprs) updT
     return retT
checkLExpr addFun (App ident False lExprs) _ =
  do (TypeSig ancTs updT retT) <- funTypeSig ident
     when ((length ancTs) + 1 /= length lExprs) $ throwError "differnet length"
     sequence $ zipWith (checkLExpr checkAncillae) (init lExprs) ancTs
     checkLExpr addFun (last lExprs) retT
     return updT






errorTypeMatch :: Ident -> BType -> LExpr -> TCError ()
errorTypeMatch i_def btype lExpr =
  case getLExprType lExpr of
    Nothing  -> fail $ "errorTypeMatch"
    (Just t) -> fail $
      "In " ++ ppIdentFile i_def ++ " function " ++ ppIdent i_def ++ " (" ++ ppIdentLine i_def ++ ") " ++
      "the type of left-expression \n  " ++ ppLExpr lExpr ++ "\nof type\n  " ++ (ppBType t) ++
      "\ndoes not match type signature \n  " ++ ppBType btype ++ "\n"

errorDifferentNumberArgs :: Ident -> Ident -> TCError ()
errorDifferentNumberArgs i_def i_sig = fail $
  "In " ++ ppIdentFile i_def ++ " function " ++ ppIdent i_def ++ " (" ++ ppIdentLine i_def ++
  ") has different number of arguments than in type signature (" ++ ppIdentLine i_sig ++ ").\n"

errorNoTypeSignature :: Ident -> TCError ()
errorNoTypeSignature i = fail $
  "In " ++ ppIdentFile i ++ " function " ++ ppIdent i ++ " (" ++ ppIdentPos i ++ ") has not type signature.\n" ++
  "  Type inference is not supported yet."
