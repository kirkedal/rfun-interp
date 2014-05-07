-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Michael Kirkedal Thomsen, 2013
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen <kirkedal@acm.org>
-- Stability   :  
-- Portability :
--
-- |Implementation a simple parser for RFun
--
-----------------------------------------------------------------------------

module Parser(parseFile, parseString, parseValue) where

import Ast
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)
import Control.Monad
import Data.Char
import Data.List

type Parser = ReadP

lexeme :: Parser a -> Parser a
lexeme p = p <* skipAll

skipAll :: Parser ()
skipAll = skipMany $ choice [comment, space]

symbol :: String -> Parser String
symbol = lexeme . string

token :: Parser a -> Parser a
token p = lexeme p

space :: Parser String
space = munch1 isSpace

comment :: Parser String
comment = string "--" >> manyTill anyChar (char '\n')
  where anyChar = satisfy (\_ -> True)

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

upperLetter :: Parser Char
upperLetter = satisfy isUpper

lowerLetter :: Parser Char
lowerLetter = satisfy isLower

keyword :: String -> Parser String
keyword = token . string

parens :: Parser a -> Parser a
parens p = symbol "(" *> p <* symbol ")"

parensT :: Parser a -> Parser a
parensT p = symbol "{" *> p <* symbol "}"

parensB :: Parser a -> Parser a
parensB p = symbol "|" *> p <* symbol "|"

parensS :: Parser a -> Parser a
parensS p = symbol "[" *> p <* symbol "]"

keywords :: [String]
keywords = ["let", "rlet", "case", "of", "in"]

ident :: Parser Ident
ident = token $ do
         k <- (:) <$> lowerLetter <*> munch (\c -> isAlphaNum c || c == '_')
         if k `elem` keywords then pfail else return k


constrName :: Parser Ident
constrName = token $ do
         k <- (:) <$> upperLetter <*> munch (\c -> isAlphaNum c || c == '_')
         if k `elem` keywords then pfail else return k

cases :: Parser [(LExpr, Expr)]
cases = some case'
  where case' = (,) <$> lexpr <*> (symbol "->" *> expr)

expr :: Parser Expr
expr = LetIn  <$> (keyword "let" *> lexpr)
              <*> (symbol "=" *> ident)
              <*> lexpr
              <*> (keyword "in" *> expr)
   <|> RLetIn <$> (keyword "rlet" *> lexpr) 
              <*> (symbol "=" *> ident)
              <*> lexpr
              <*> (keyword "in" *> expr)
   <|> CaseOf <$> (keyword "case" *> lexpr <* keyword "of")
              <*> cases
   <|> LeftE <$> lexpr

lexpr :: Parser LExpr
lexpr = (\x y -> Constr "Tuple" [x,y]) <$> (symbol "{" *> lexpr) <*> (symbol "," *> lexpr <* symbol "}")
    <|> (\x -> Constr "Tuple" [x]) <$> parensT lexpr
    <|> DupEq  <$> parensB lexpr
    <|> Constr <$> constrName <*> parens vars 
    <|> Constr <$> constrName <*> pure []
    <|> Var    <$> ident
    <|> parens (chainr1 lexpr cons)
    <|> symbol "[]" *> pure (Constr "Nil" [])
    <|> parens lexpr 
    where
      cons = symbol ":" *> pure (\v1 v2 -> Constr "Cons" [v1,v2])

someVars :: Parser [LExpr]
someVars = lexpr `sepBy1` symbol ","

vars :: Parser [LExpr]
vars = someVars <|> pure []


funDef :: Parser Func
funDef = Func <$> ident <*> lexpr <*> 
         (symbol "=^=" *> expr )


funDefs :: Parser [Func]
funDefs = many funDef

program :: Parser Program
program = funDefs


-- Parsing values

value :: Parser Value
value = (\x y -> ConstrV "Tuple" [x,y]) <$> (symbol "{" *> value) <*> (symbol "," *> value <* symbol "}")
    <|> (\x -> ConstrV "Tuple" [x]) <$> parensT value
    <|> ConstrV <$> constrName <*> parens vals 
    <|> ConstrV <$> constrName <*> pure []
    <|> parens (chainr1 value cons)
    <|> symbol "[]" *> pure (ConstrV "Nil" [])
    where
      cons = symbol ":" *> pure (\v1 v2 -> ConstrV "Cons" [v1,v2])

someVals :: Parser [Value]
someVals = value `sepBy1` symbol ","

vals :: Parser [Value]
vals = someVals <|> pure []

parseValue :: String -> Either Error Value
parseValue s = case find (null . snd) $ readP_to_S (skipAll *> value) s of
                  Just (x,_) -> Right x
                  Nothing    -> Left "syntax error in value"


parseString :: String -> Either Error Program
parseString s = case find (null . snd) $ readP_to_S (skipAll *> program) s of
                  Just (x,_) -> Right x
                  Nothing    -> Left "syntax error"

parseFile :: FilePath -> IO (Either Error Program)
parseFile = liftM parseString . readFile
