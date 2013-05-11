{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.Monoid
import Data.ByteString.UTF8 as UTF8
import Control.Applicative
import Text.Trifecta

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data TypeTree a
  = TypeName a
  | TypeValue a
  | SubTree [TypeTree a]
  deriving (Show, Read, Eq, Functor)

data TypeSentence a 
  = TApply [TypeSentence a] 
  | TTree [TypeTree a] deriving (Show, Eq, Functor)

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

--parse実行
parseTypeSentence :: String -> IO (Maybe (TypeSentence String))
parseTypeSentence s 
  = case parseByteString (typeApplySentence <* eof) mempty (UTF8.fromString s) of
    Failure xs -> print xs >> return Nothing
    Success a  -> return $ Just a

--------

typeName :: Parser (TypeTree String)
typeName = TypeName <$> word ((:) <$> upper <*> many letter)

typeValue :: Parser (TypeTree String)
typeValue = TypeValue <$> word (many1 letter)

subTree :: Parser (TypeTree String)
subTree = SubTree <$> word (parens typeSyntax)

typeSyntax :: Parser [TypeTree String]
typeSyntax = sepBy (subTree <|> typeName <|> typeValue) (symbol "->")

typeApplySentence :: Parser (TypeSentence String)
typeApplySentence 
  = TApply <$> many1 (braces typeApplySentence) <|> TTree <$> typeSyntax

--------

--こういうのなんかありそう
word :: Parser a -> Parser a
word p = spaces *> p  <* spaces

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

-------------------------------------------------------------------------------
-- Type inference
-------------------------------------------------------------------------------
-- アルファ変換／ベータ簡約

alpha :: TypeSentence a -> TypeSentence a -> TypeSentence a
alpha = undefined

beta :: TypeSentence a -> Either (TypeSentence a) (TypeSentence a)
beta = undefined

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
   putStrLn "--------------------------------------"
   t "a -> b -> a"
   t "a -> b c -> a"
   t "{{a -> b} {a -> c}} {c -> d}"
   t "{(a -> b) -> (a -> c)} {c -> d}"
   t "{(a -> b) -> (a -> c) -> a -> c} {c -> d}"
   t "{(a -> b) -> (a -> c) -> c} {c -> d}"
   t "{(a -> b) (a -> c) c} {c -> d}"

   putStrLn "--------------------------------------"
   x <- parseTypeSentence "c -> a b -> b"
   print x
     where
       t = parseTest (typeApplySentence <* eof) 
