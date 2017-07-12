-----------------------------------------------------------------------------
--
-- Module      :  Parser
-- Copyright   :  Michael Kirkedal Thomsen, 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen <kirkedal@acm.org>
-- Stability   :  None guaranteed?
-- Portability :
--
-- |Implementation a simple parser for rFun
--
-----------------------------------------------------------------------------


module Parser (parseString, parseFromFile, parseValue, ParseError) where


import Text.ParserCombinators.Parsec hiding (parse,parseFromFile)
import qualified Text.Parsec.Token as P
import Text.Parsec.Prim (runP, parserFail)
import Control.Monad.Identity

import Ast

type ParserState = [String]

initialState :: ParserState
initialState = []
type LangParser = GenParser Char ParserState

-------------------------------------------------------------------------------
-- * Functions for parsing values and programs
-------------------------------------------------------------------------------

--parse' p = parse p "could not parse"
parse :: LangParser a -> String -> String -> Either ParseError a
parse p = runP p initialState

-- |Parse a RFun program from a file
parseFromFile :: String -> IO (Either ParseError Program)
parseFromFile fname
    = do input <- readFile fname
         return (parse program fname input)

-- |Parse a RFun program from a string
parseString :: String -> IO (Either ParseError Program)
parseString input = return $ parse program "Text field" input

-- |Parse a RFun value from a string
parseValue :: String -> IO (Either ParseError Value)
parseValue input = return $ parse value "Value" input

-------------------------------------------------------------------------------
-- * Implementation of the parser
-------------------------------------------------------------------------------

cStyle :: P.GenLanguageDef String st Identity
cStyle = P.LanguageDef {
  P.commentStart    = "",
  P.commentEnd      = "",
  P.commentLine     = "--",
  P.nestedComments  = False,
  P.identStart      = letter <|> char '_',
  P.identLetter     = alphaNum <|> char '_' <|> char '\'',
  P.opStart         = oneOf "=-|",
  P.opLetter        = oneOf "^=>|-",
  P.reservedOpNames = ["=^=", "=", "->", "#"],
  P.reservedNames   = ["let", "rlet", "in", "case", "of", "end"],
  P.caseSensitive   = True
}

-- |Used to conveniently create the parsers 'natural', 'constant', and 'identifier'
lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser cStyle

-- |Parses a natural number
natural :: CharParser st Integer
natural = P.natural lexer

lexeme :: CharParser st a -> CharParser st a
lexeme = P.lexeme lexer

-- |Parses white space
whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

-- |Parses the string s and skips trailing whitespaces
symbol :: String -> CharParser st String
symbol = P.symbol lexer

-- |Parses and returns a valid identifier
identifier :: CharParser st String
identifier = P.identifier lexer

-- |Parser @(parens p)@ parses p and trailing whitespaces enclosed in parenthesis ('(' and ')'),
--  returning the value of p.
parens :: CharParser st a -> CharParser st a
parens = P.parens lexer

-- |Parser @(brackets p)@ parses p and trailing whitespaces enclosed in square brackets ('[' and ']'),
--  returning the value of p.
brackets :: CharParser st a -> CharParser st a
brackets = P.squares lexer

-- |Parser @(brackets p)@ parses p and trailing whitespaces enclosed in square brackets ('{' and '}'),
--  returning the value of p.
braces :: CharParser st a -> CharParser st a
braces = P.braces lexer

reserved :: String -> CharParser st ()
reserved = P.reserved lexer

-- |Parses a constant (i.e. a number)
--
-- Looks kinda useless but the definition of constant is not fixed
constant :: CharParser st Int
constant = lexeme natural >>= return . fromIntegral

--eol = string "\n"

program :: LangParser  Program
program =
  do  whiteSpace
      f <- many1 funDef
      eof
      return f

funDef :: LangParser Func
funDef =
  do  i <- identifier
      symbol "::"
      t <- typeSig
      cases <- many $ funDefPart i
      when (cases == []) (parserFail $ "Unaccompanied type description (" ++ i ++ ")")
      return $ Func i t (Var "_ctmp") (CaseOf (Var "_ctmp") cases)

funDefPart :: String -> LangParser (LExpr, Expr)
funDefPart ident =
  do  i <- lookAhead identifier
      when (i /= ident) (parserFail $ "Function name (" ++ i ++") does not match type descriptor (" ++ ident ++ ")")
      identifier
      les <- many1 lexpr
      symbol "="
      e <- expr
      return $ ((wrapLExpr les), appLeaves (\x -> wrapLExpr $ (init les)++[x]) e)


wrapLExpr :: [LExpr] -> LExpr
wrapLExpr [x] = x
wrapLExpr xs  = Constr "Tuple" xs

typeSig :: LangParser TypeSig
typeSig =
  do  at <- many $ try eType -- Ancillae types
      lt <- btype        -- left type
      symbol "=>"
      rt <- btype        -- right type
      return $ TypeSig at lt rt
  where
    eType = do t <- extType
               symbol "->"
               return t

extType :: LangParser BType
extType = try btype <|> funT
  where
    funT  = do t <- parens typeSig
               return $ FunT t

btype :: LangParser BType
btype = try nat <|> try list <|> try tuple <|> try anyT <|> parens btype
  where
    nat   = symbol "Nat" >> return Nat
    list  = do t <- brackets btype
               return $ List t
    tuple = do t <- parens $ sepBy btype (symbol ",")
               return $ Tup t
    anyT  = do i <- identifier
               return $ Any i

expr :: LangParser Expr
expr = try letin <|> try rletin <|> try caseofF <|> try caseof <|> try apply <|> lefte
    where
        letin  = do reserved "let"
                    l <- many1 assign
                    e <- choice [inPart, letin, rletin]
                    return $ foldr (\(lExpr1, ident, lExpr2) ex -> LetIn lExpr1 ident lExpr2 ex) e l
        rletin = do reserved "rlet"
                    l <- many1 assign
                    e <- choice [inPart, letin, rletin]
                    return $ foldr (\(lExpr1, ident, lExpr2) ex -> RLetIn lExpr1 ident lExpr2 ex) e l
        inPart = do reserved "in"
                    e <- expr
                    reserved "end"
                    return $ e
        assign = do leout <- lexpr
                    symbol "="
                    lookAhead (lower)
                    fun <- identifier
                    lein <- many1 $ try alexpr
                    return $ ((wrapLExpr $ init lein++[leout]), fun, wrapLExpr lein)
        alexpr = do l <- lexpr
                    notFollowedBy $ choice [symbol "=", symbol "::"]
                    return l
        caseofF= do reserved "case"
                    lookAhead (lower)
                    fun <- identifier
                    le <- lexpr
                    reserved "of"
                    c <- many1 $ try cases
                    return $ LetIn (Var "_tmp") fun le (CaseOf (Var "_tmp") c)
        caseof = do reserved "case"
                    le <- lexpr
                    reserved "of"
                    c <- many1 $ try cases
                    return $ CaseOf le c
        lefte  = do le <- lexpr
                    return $ LeftE le
        apply  = do lookAhead (lower)
                    fun <- identifier
                    le <- many1 alexpr
                    return $ LetIn (wrapLExpr $ init le ++ [Var "_tmp"]) fun (wrapLExpr le) (LeftE (Var "_tmp"))
        cases  = do le <- lexpr
                    symbol "->"
                    e <- expr
                    return (le,e)

constToConstr :: Int -> LExpr
constToConstr n
    | n == 0    = Constr "Z" []
    | n <  0    =  Constr "P" [constToConstr (n+1)]
    | otherwise =  Constr "S" [constToConstr (n-1)]

lexpr :: LangParser LExpr
lexpr = try consta <|> try vari <|> try tuple <|> try dupeq <|> try constr <|> try constrN <|> try list1 <|> try list2 <|> parenE
    where
        consta = do c <- constant
                    return $ constToConstr c
        vari   = do lookAhead (lower)
                    var <- identifier
                    return $ Var var
        tuple  = do les <- braces $ lexpr `sepBy` (symbol ",")
                    return $ Constr "Tuple" les
        dupeq  = do symbol "|"
                    le <- lexpr
                    symbol "|"
                    return $ DupEq le
        constr = do lookAhead (upper)
                    i <- identifier
                    vars <- parens $ lexpr `sepBy` (symbol ",")
                    return $ Constr i vars
        constrN= do lookAhead (upper)
                    i <- identifier
                    return $ Constr i []
        list1  = parens $ chainr1 lexpr cons
        cons   = do symbol ":"; return (\v1 v2 -> Constr "Cons" [v1,v2])
        list2  = do l <- brackets $ lexpr `sepBy` (symbol ",")
                    return $ foldr (\a b -> Constr "Cons" [a,b]) (Constr "Nil" []) l
        parenE = parens lexpr


-------------------------------------------------------------------------------
-- * Parsing values
-------------------------------------------------------------------------------

constToValue :: Int -> Value
constToValue n
    | n == 0    = ConstrV "Z" []
    | n <  0    = ConstrV "P" [constToValue (n+1)]
    | otherwise = ConstrV "S" [constToValue (n-1)]

value :: LangParser Value
value = do vs <- many1 bvalue
           return $ wrap vs
  where 
    wrap [x] = x
    wrap xs  = ConstrV "Tuple" xs

bvalue :: LangParser Value
bvalue = try consta <|> try tuple <|> try constr <|> try constrN <|> try list <|> parenV
    where
        consta = do c <- constant
                    return $ constToValue c
        tuple  = do les <- braces $ bvalue `sepBy` (symbol ",")
                    return $ ConstrV "Tuple" les
        constr = do i <- identifier
                    vars <- parens $ bvalue `sepBy` (symbol ",")
                    return $ ConstrV i vars
        constrN= do i <- identifier
                    return $ ConstrV i []
        list   = do l <- brackets $ bvalue `sepBy` (symbol ",")
                    return $ foldr (\a b -> ConstrV "Cons" [a,b]) (ConstrV "Nil" []) l
        parenV = parens bvalue

