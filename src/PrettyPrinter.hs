---------------------------------------------------------------------------
--
-- Module      :  PrettyPrinter
-- Copyright   :  Michael Kirkedal Thomsen, 2017
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen <kirkedal@acm.org>
-- Stability   :  none?
-- Portability :  ?
--
-- |Pretty printer for RFun17
--
-----------------------------------------------------------------------------

module PrettyPrinter (ppProgram, ppTypeSig, ppBType, ppLExpr, ppIdent, ppIdentFile, ppIdentPos, ppIdentLine, ppValue) where

import Ast

import Text.PrettyPrint
import Data.List (intersperse)
import Text.Megaparsec (sourceName, sourceLine, sourceColumn, Pos, unPos)
import qualified Data.Map as M
-- import Text.Megaparsec.Pos (unPos)

-- import Data.Complex

ppProgram :: Program -> String
ppProgram = render . formatProgram

ppBType :: BType -> String
ppBType = render . formatBType

ppTypeSig :: TypeSig -> String
ppTypeSig = render . formatTypeSig

ppLExpr :: LExpr -> String
ppLExpr = render . formatLExpr

ppIdent :: Ident -> String
ppIdent = render . formatIdent

ppIdentFile :: Ident -> String
ppIdentFile = sourceName . sourcePos

ppIdentPos :: Ident -> String
ppIdentPos i = "line " ++ (showPos $ sourceLine $ sourcePos i) ++ ", column " ++ (showPos $ sourceColumn $ sourcePos i)

ppIdentLine :: Ident -> String
ppIdentLine i = "line " ++ (showPos $ sourceLine $ sourcePos i)

ppValue :: Value -> String
ppValue = render . formatValue


showPos :: Pos -> String
showPos = show . unPos

formatProgram :: Program -> Doc
formatProgram p =
  (vcat $ intersperse (text "") $ map formatFunc p)

formatFunc :: Func -> Doc
formatFunc func@(Func _ _ _) =
  case funcTypesig func of
    Nothing    -> vcat $ map (formatClause (funcName func)) $ funcClause func
    (Just sig) -> formatIdent (funcName func) <+> text "::" <+> formatTypeSig sig $+$
                  (vcat $ map (formatClause (funcName func)) $ funcClause func)
formatFunc (DataType dataN dataD) =
  text "data" <+> formatIdent dataN <+> text "=" <+>
    (hsep $ intersperse (text "|") $ map (\(_,(i,b)) -> formatIdent i <+> (hsep (map (formatBType) b))) (M.toList dataD))

formatClause :: Ident -> Clause -> Doc
formatClause ident clause =
  case clauseBody clause of
    (LeftE l) -> formatIdent ident <+>
                   (hsep $ map formatLExpr (clauseParam clause)) <+>
                   formatGuard (clauseGuard clause) <+>
                   text "=" <+> formatLExpr l
    e         -> formatIdent ident <+>
                   (hsep $ map formatLExpr (clauseParam clause)) <+>
                   formatGuard (clauseGuard clause) <+>
                   text "=" $+$ (nest 2 $ formatExpr e)

formatGuard :: Guard -> Doc
formatGuard (Guard []) = empty
formatGuard (Guard g)  = text "|" <+> commaSep formatLExpr g

formatTypeSig :: TypeSig -> Doc
formatTypeSig (TypeSig ancT leftT rightT) =
  (hcat $ map (\x -> formatBType x <> text " -> ") ancT) <> formatBType leftT <+> text "<->" <+> formatBType rightT

formatBType :: BType -> Doc
-- formatBType NatT           = text "Nat"
formatBType  IntT          = text "Int"
formatBType  QbitT         = text "Qbit"
formatBType (DataT ident)  = formatIdent ident
formatBType (ListT bType)  = brackets $ formatBType bType
formatBType (ProdT bTypes) = parens $ commaSep formatBType bTypes
formatBType (SumT  bTypes) = parens $ hsep $ intersperse (text "+") $ map formatBType bTypes
formatBType (FunT typeSig) = parens $ formatTypeSig typeSig
formatBType (VarT ident)   = formatIdent ident
formatBType (AnyT)         = text "a" -- Introduce new vars


formatExpr :: Expr -> Doc
formatExpr (LeftE le)      = formatLExpr le
formatExpr e@(LetIn _ _ _) = formatLetIn False e
  where
    formatLetIn True (LetIn left right expr@(LetIn _ _ _)) =
      formatLExpr left <+> text "=" <+> formatLExpr right $+$ formatLetIn True expr
    formatLetIn True (LetIn left right expr) = -- Not followed by let-in
      formatLExpr left <+> text "=" <+> formatLExpr right $+$ (nest (-2) $ text "in") $+$ formatExpr expr
    formatLetIn False (LetIn left right expr@(LetIn _ _ _)) =
      text "let" $+$ nest 2 (formatLExpr left <+> text "=" <+> formatLExpr right $+$ formatLetIn True expr)
    formatLetIn False (LetIn left right expr) = -- Not followed by let-in
      text "let" $+$ nest 2 (formatLExpr left <+> text "=" <+> formatLExpr right $+$ (nest (-2) $ text "in") $+$ formatExpr expr)
    formatLetIn _ _ = error "... In format of Expr"
formatExpr (CaseOf lexpr cases) =
  text "case" <+> formatLExpr lexpr <+> text "of" $+$ (nest 2 $ vcat $ map formatcase cases)
  where
    formatcase (l, _, (LeftE le)) = formatLExpr l <+> text "->" <+> formatLExpr le
    formatcase (l, _, e) = formatLExpr l <+> text "->" $+$ (nest 2 $ formatExpr e)
    -- No guard

formatLExpr :: LExpr -> Doc
formatLExpr (Var ident)            = formatIdent ident
formatLExpr (Constr ident [])      = formatIdent ident
formatLExpr (Constr ident lExprs)  = parens $ formatIdent ident <+> hsep (map formatLExprc lExprs)
formatLExpr (Int i)                = integer i
formatLExpr (Tuple lExprs)         = parens $ commaSep formatLExpr lExprs
formatLExpr (List l) | nilTerm l   = brackets $ commaSep formatLExpr $ tolist l
formatLExpr (List l)               = parens $ colonSep formatLExprc $ tolist l
formatLExpr (App ident rev lExprs) = formatIdent ident <> formatRev rev <+> hsep (map formatLExprc lExprs)
  where
    formatRev True  = text ""
    formatRev False = text "!"

formatLExprc :: LExpr -> Doc
formatLExprc (Var ident)            = formatIdent ident
formatLExprc (Constr ident [])      = formatIdent ident
formatLExprc (Constr ident lExprs)  = parens $ formatIdent ident <+> hsep (map formatLExpr lExprs)
formatLExprc (Int i)                = integer i
formatLExprc (Tuple lExprs)         = parens $ commaSep formatLExpr lExprs
formatLExprc (List l) | nilTerm l   = brackets $ commaSep formatLExpr $ tolist l
formatLExprc (List l)               = parens $ colonSep formatLExpr $ tolist l
formatLExprc (App ident rev lExprs) = parens $ formatIdent ident <> formatRev rev <+> hsep (map formatLExpr lExprs)
  where
    formatRev True  = text ""
    formatRev False = text "!"


tolist :: LExprList -> [LExpr]
tolist (ListCons l ls) = l : (tolist ls)
tolist (ListNil)       = []
tolist (ListEnd l)     = [l]

nilTerm :: LExprList -> Bool
nilTerm (ListCons _ list) = nilTerm list
nilTerm ListNil        = True
nilTerm _              = False

formatIdent :: Ident -> Doc
formatIdent = text.identifier

formatValue :: Value -> Doc
formatValue (IntV i)  = integer i
formatValue (QbitV c)  = text $ show c
formatValue (TupleV values) = parens $ commaSep formatValue values
formatValue (ListV  values) = brackets $ commaSep formatValue values
formatValue (ConstrV ident values) = parens $ text ident <+> (hsep (map formatValue values))
formatValue (FunV i) = text i


commaSep :: (a -> Doc) -> [a] -> Doc
commaSep f x = hcat $ intersperse (text ", ") $ map f x

colonSep :: (a -> Doc) -> [a] -> Doc
colonSep f x = hcat $ intersperse (text ":") $ map f x

