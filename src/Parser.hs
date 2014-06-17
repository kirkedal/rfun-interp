-----------------------------------------------------------------------------
--
-- Module      :  Parser
-- Copyright   :  Michael Kirkedal Thomsen, 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen <kirkedal@acm.org>
-- Stability   :  
-- Portability :
--
-- |Implementation a simple parser for rFun
--
-----------------------------------------------------------------------------


module Parser (parseString, parseFromFile, parseValue, ParseError) where


import Text.ParserCombinators.Parsec hiding (parse,parseFromFile)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language
import qualified Text.Parsec.Char as PC
import Text.Parsec.Prim (runP)
import qualified Text.ParserCombinators.Parsec.Expr as E

import Control.Monad
import Data.Maybe

import Ast

type ParserState = [String]

initialState :: ParserState
initialState = []
type LangParser = GenParser Char ParserState

-------------------------------------------------------------------------------
-- * Functions for parsing values and programs
-------------------------------------------------------------------------------

parse' p = parse p "could not parse"
parse p = runP p initialState

parseFromFile :: String -> IO (Either ParseError Program)
parseFromFile fname
    = do input <- readFile fname
         return (parse program fname input)

parseString :: String -> IO (Either ParseError Program)
parseString input = return $ parse program "Text field" input

parseValue :: String -> IO (Either ParseError Value)
parseValue input = return $ parse value "Value" input

-------------------------------------------------------------------------------
-- * Implementation of the parser
-------------------------------------------------------------------------------

cStyle = P.LanguageDef {
  P.commentStart    = "",
  P.commentEnd      = "",
  P.commentLine     = "--",
  P.nestedComments  = False, 
  P.identStart      = letter <|> char '_', 
  P.identLetter     = alphaNum <|> char '_' <|> char '\'',
  P.opStart         = oneOf "=-|", 
  P.opLetter        = oneOf "^=>|-", 
  P.reservedOpNames = ["=^=", "=", "->"],
  P.reservedNames   = ["let", "rlet", "in", "case", "of"],
  P.caseSensitive   = True
}

-- |Used to conveniently create the parsers 'natural', 'constant', and 'identifier'
lexer = P.makeTokenParser cStyle

-- |Parses a natural number
natural = P.natural lexer

-- |Parses a colon
colon = P.colon lexer

lexeme = P.lexeme lexer

-- |Parses the character ',' and skips any trailing white space. Returns the string ",".
comma = P.comma lexer

-- |Parses white space
whiteSpace = P.whiteSpace lexer

-- |Parses the string s and skips trailing whitespaces
symbol :: String -> CharParser st String
symbol = P.symbol lexer

-- |Parses and returns a valid identifier
identifier :: CharParser st String
identifier = P.identifier lexer

-- |Parser @(parens p)@ parses p and trailing whitespaces enclosed in parenthesis ('(' and ')'),
--  returning the value of p.
parens = P.parens lexer

-- |Parser @(brackets p)@ parses p and trailing whitespaces enclosed in square brackets ('[' and ']'),
--  returning the value of p.
brackets = P.squares lexer

braces = P.braces lexer

reserved = P.reserved lexer

-- |Parses a constant (i.e. a number)
--
-- Looks kinda useless but the definition of constant is not fixed
--constant :: CharParser st Const
constant = lexeme natural >>= return . fromIntegral

--eol = string "\n"

program :: LangParser Program
program = whiteSpace >> many1 funDef


funDef :: LangParser Func
funDef = do i <- identifier
            le <- lexpr
            symbol "=^="
            e <- expr
            return $ Func i le e

expr :: LangParser Expr
expr = try letin <|> try rletin <|> try caseof <|> try apply <|> lefte
    where
        letin  = do reserved "let"
                    l <- many1 assign
                    reserved "in"
                    e <- expr
                    return $ foldr (\(lExpr1, ident, lExpr2) e -> LetIn lExpr1 ident lExpr2 e) e l
        rletin = do reserved "rlet"
                    l <- many1 assign
                    reserved "in"
                    e <- expr
                    return $ foldr (\(lExpr1, ident, lExpr2) e -> RLetIn lExpr1 ident lExpr2 e) e l
        assign = do leout <- lexpr ; 
                    symbol "=" ; 
                    fun <- identifier ; 
                    lein <- lexpr ; 
                    return $ (leout, fun, lein) 
        caseof = do reserved "case"
                    le <- lexpr
                    reserved "of"
                    c <- many1 $ try cases
                    return $ CaseOf le c
        lefte  = do le <- lexpr
                    return $ LeftE le
        apply  = do lookAhead (lower)
                    fun <- identifier
                    le <- lexpr
                    notFollowedBy $ lexpr >> symbol "=^="
                    return $ LetIn (Var "_tmp") fun le (LeftE (Var "_tmp"))
        cases  = do le <- lexpr
                    symbol "->"
                    e <- expr
                    return (le,e)

constToConstr 0 = Constr "Z" []
constToConstr n = Constr "S" [constToConstr (n-1)]

lexpr :: LangParser LExpr
lexpr = try const <|> try var <|> try tuple <|> try dupeq <|> try constr <|> try constrN <|> try list1 <|> try list2 <|> parenE
    where
        const  = do c <- constant
                    return $ constToConstr c
        var    = do lookAhead (lower)
                    var <- identifier
                    return $ Var var
        tuple  = do les <- braces $ lexpr `sepBy1` (symbol ",")
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




-- Parsing values

constToValue 0 = ConstrV "Z" []
constToValue n = ConstrV "S" [constToValue (n-1)]

value :: LangParser Value
value = try const <|> try tuple <|> try constr <|> try constrN <|> try list <|> parenV
    where
        const  = do c <- constant
                    return $ constToValue c
        tuple  = do les <- braces $ value `sepBy1` (symbol ",")
                    return $ ConstrV "Tuple" les
        constr = do i <- identifier
                    vars <- parens $ value `sepBy` (symbol ",")
                    return $ ConstrV i vars
        constrN= do i <- identifier
                    return $ ConstrV i []
        --list1  = parens $ chainr1 value cons
        --cons   = do symbol ":"; pure (\v1 v2 -> ConstrV "Cons" [v1,v2])
        list   = do l <- brackets $ value `sepBy` (symbol ",")
                    return $ foldr (\a b -> ConstrV "Cons" [a,b]) (ConstrV "Nil" []) l
        parenV = parens value
        
