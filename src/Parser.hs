
module Parser (parseFromFile, parseFromValue, prettyParseError, ParserError) where

import Ast

import Control.Applicative (empty)
import Control.Monad (void, guard, when, forM_)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import qualified Data.Map as M

type ParserError = ParseError Char Dec

prettyParseError :: ParserError -> String
prettyParseError = parseErrorPretty

parseFromFile :: FilePath -> IO (Either ParserError Program)
parseFromFile fname =
  do input <- readFile fname
     return (parse programParser fname input)
     -- return (parse programParser fname input)

parseFromValue :: String -> Either ParserError Value
parseFromValue s = parse pValue "Value" s

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

programParser :: Parser Program
programParser = pProgram <* scn <* eof

---

integer :: Parser Integer
integer = lexeme $ L.signed sc L.integer


symbol :: String -> Parser ()
symbol s = L.symbol' sc s >> return ()

reservedWords :: [String]
reservedWords = ["let", "in", "case", "of", "data", "eq", "id"]

reserved :: String -> Parser ()
reserved s = lexeme $ (string s >> return ())

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

pIdentifier :: Parser Ident
pIdentifier = (lexeme $ p)
  where
    p = do
      lookAhead lowerChar
      pos <- getPosition
      str <- some (alphaNumChar <|> char '-' <|> char '_' <|> char '\'' <?> "identifier that is not a keyword: " ++ show reservedWords)
      guard (not $ elem str reservedWords)
      return $ Ident str pos

pConstructor :: Parser Ident
pConstructor = lexeme $ p
  where
    p = do
      lookAhead upperChar
      pos <- getPosition
      str <- some (alphaNumChar <|> char '-' <|> char '_' <?> "constructor name")
      return $ Ident str pos

--

pProgram :: Parser Program
pProgram = some $ (try pDataType <|> pFunDef)

pFunDef :: Parser Func
pFunDef =
  do (idt, tps) <- try pTypeSig <?> "function signature"
     cls <- (some $ try (pFunDefClause $ identifier idt))
     return $ Func idt tps cls

pDataType :: Parser Func
pDataType = L.nonIndented scn $ L.lineFold scn p
  where
    p sc' =
      do reserved "data" <?> "data type"
         i <- pConstructor
         symbol "="
         sc'
         ts <- item `sepBy1` (symbol "|")
         return $ DataType i (M.fromList $ map (\x -> (identifier $ fst x, x)) ts)
    item =
      do i  <- pConstructor
         ts <- many btype
         return (i, ts)


pTypeSig :: Parser (Ident, Maybe TypeSig)
pTypeSig = L.nonIndented scn p
  where
    p = do
      idt <- lookAhead pIdentifier
      tps <- optional $ try t
      return (idt, tps)
    t = do
      pIdentifier
      symbol "::"
      typeSig

typeSig :: Parser TypeSig
typeSig =
  do  at <- many $ try eType -- Ancillae types
      lt <- btype        -- left type
      symbol "<->"
      rt <- btype        -- right type
      return $ TypeSig at lt rt
  where
    eType = do t <- extType
               symbol "->"
               return t

extType :: Parser BType
extType = try btype <|> funT
  where
    funT  = do t <- parens typeSig
               return $ FunT t

btype :: Parser BType
btype = try list <|> try tuple <|> try dataT <|> try anyT <|> parens btype
  where
    -- nat   = symbol "Nat" >> return NatT
    list  = do t <- brackets btype
               return $ ListT t
    tuple = do t <- parens $ sepBy btype (symbol ",")
               return $ ProdT t
    dataT = do i <- pConstructor
               return $ DataT i
    anyT  = do i <- pIdentifier
               return $ VarT i

pFunDefClause :: String -> Parser Clause
pFunDefClause iTps = L.nonIndented scn $ L.lineFold scn p
  where
    p sc' = do  idt <- pIdentifier
                guard ((identifier idt) == iTps)
                les <- some pLexpr
                gs <- pGuard
                symbol "="
                sc'
                e <- pExpr
                return $ Clause idt les gs e

pGuard :: Parser Guard
pGuard = try someguard <|> (return $ Guard [])
  where
    someguard =
      do  symbol "|"
          gs <- sepBy1 pLexprA (symbol ",")
          return $ Guard gs

pExpr :: Parser Expr
pExpr = try (L.lineFold scn letin) <|> try caseof <|> lefte <?> "expression"
  where
    letin sc'  = do il <- L.indentLevel
                    reserved "let"
                    sc'
                    il_l <- L.indentLevel
                    l <- some $ try assign
                    forM_ l (\(_,_,i) -> when (il_l /= i) (L.incorrectIndent EQ il_l i))
                    scn
                    L.indentGuard sc EQ il
                    reserved "in"
                    sc'
                    e <- pExpr
                    return $ foldr (\(lExpr1, lExpr2, _) ex -> LetIn lExpr1 lExpr2 ex) e l
    assign     = do scn
                    il <- L.indentLevel
                    leout <- pLexpr
                    symbol "="
                    lein <- pLexprA
                    return $ (leout, lein, il)
    caseof     = do il <- L.indentLevel
                    reserved "case"
                    le <- pLexprA
                    reserved "of"
                    scn
                    L.indentGuard scn GT il
                    il_l <- L.indentLevel
                    c <- some $ try cases
                    forM_ c (\(_,_,_,i) -> when (il_l /= i) (L.incorrectIndent EQ il_l i))
                    return $ CaseOf le $ map (\(x,y,z,_) -> (x,y,z)) c
    cases      = do scn
                    il <- L.indentLevel
                    le <- pLexpr
                    -- gs <- guard
                    symbol "->"
                    e <- pExpr
                    return (le, (Guard []), e, il)
    lefte  = do le <- pLexprA
                return $ LeftE le

-- |Parsing a basic left-expression
pLexpr :: Parser LExpr
pLexpr = constr <|> vari <|> int <|> listf <|> try (parens constrp) <|> try tuple <|> try listc <|> parLE <?> "Left-expression"
  where
    int    = do i <- integer
                return $ Int i
    vari   = do var <- pIdentifier
                return $ Var var
    constr = do c <- pConstructor
                return $ Constr c []
    constrp= do c <- pConstructor
                vars <- many pLexpr
                return $ Constr c vars
    tuple  = do les <- parens $ pLexpr `sepBy` (symbol ",")
                return $ Tuple les
    listc  = do les <- parens $ pLexpr `sepBy1` (symbol ":")
                return $ List $ foldr (\a b -> ListCons a b) (ListEnd $ last les) (init les)
    listf  = do les <- brackets $ pLexpr `sepBy` (symbol ",")
                return $ List $ foldr (\a b -> ListCons a b) ListNil les
    parLE  = parens pLexpr

-- |Parsing a complete left-expression; including function application
pLexprA :: Parser LExpr
pLexprA = try app <|> try parLE <|> pLexpr  <?> "Left-expression"
  where
    app    = do fun <- pIdentifier
                inv <- optional $ symbol "!"
                les <- some pLexpr
                return $ App fun (inv == Nothing) les
    parLE  = parens pLexprA


pValue :: Parser Value
pValue = int <|> tuple <|> list <|> try funName <|> try constr <|> try constrp <|> try tuple <|> par <?>  "Value"

  where
    int    = do i <- integer
                return $ IntV i
    tuple  = do vs <- parens $ pValue `sepBy` (symbol ",")
                return $ TupleV vs
    list   = do vs <- brackets $ pValue `sepBy1` (symbol ",")
                return $ ListV vs
    constr = do c <- pConstructor
                return $ ConstrV (identifier c) []
    constrp= do c <- pConstructor
                vars <- many pValue
                return $ ConstrV (identifier c) vars
    funName= do i <- pIdentifier
                return $ FunV $ identifier i
    par    = parens pValue

