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

type Names = (String, String)

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

typeInference :: Maybe (TypeSentence String) -> Maybe [TypeTree Names]
typeInference tree = do
     names <- fmap concatSentence $ fmap toPare <$> tree
     return $ (filter isTypeValue names)

--------

alpha :: TypeSentence Names -> [TypeTree Names] -> TypeTree Names 
alpha l r = let
  sentence = filter isTypeValue $ concatSentence l
  typeNames = filter isTypeValue $ concatTree r
  in undefined

beta :: TypeSentence Names -> Either (TypeSentence Names) (TypeSentence Names)
beta = undefined

-------------------------------------------------------------------------------
-- Helper functions
-------------------------------------------------------------------------------

concatTree :: [TypeTree a] -> [TypeTree a]
concatTree = concatMap f
  where
    f :: TypeTree a -> [TypeTree a]
    f (SubTree t) = t 
    f x = [x]

concatSentence :: TypeSentence a -> [TypeTree a]
concatSentence (TApply xs) = concatMap concatSentence xs
concatSentence (TTree t) = concatTree t

isTypeValue :: TypeTree a -> Bool
isTypeValue (TypeValue _) = True
isTypeValue _ = False

--------

toPare :: a -> (a, a)
toPare a = (a, a)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "--------------------------------------"
  print . typeInference =<< parseTypeSentence "{a -> Hoge -> Piyo} {Huga -> b}"
