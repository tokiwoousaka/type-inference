{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.List
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad.State
import Text.Trifecta
import qualified Data.ByteString.UTF8 as UTF8

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

data TypeTree a
  = TypeName a
  | TypeVariable a
  | SubTree [TypeTree a]
  deriving (Show, Read, Eq, Functor)

data TypeSentence a 
  = TApply [TypeSentence a] 
  | TTree [TypeTree a] deriving (Show, Eq, Functor)

type Names = (String, String)

type NamesState = State [Names]

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

lod :: Parser Char
lod = letter <|> digit

typeName :: Parser (TypeTree String)
typeName = TypeName <$> word ((:) <$> upper <*> many lod)

typeVariable :: Parser (TypeTree String)
typeVariable = TypeVariable <$> word (many1 lod)

subTree :: Parser (TypeTree String)
subTree = SubTree <$> word (parens typeSyntax)

typeSyntax :: Parser [TypeTree String]
typeSyntax = sepBy (subTree <|> typeName <|> typeVariable) (symbol "->")

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

typeInference :: TypeSentence String -> TypeSentence String
typeInference = id --TODO

--ベータ簡約
beta :: TypeSentence Names -> Either (TypeSentence Names) (TypeSentence Names)
beta = undefined

--------
-- アルファ変換
--------

alpha :: TypeSentence Names -> [TypeTree Names] -> TypeSentence Names
alpha = undefined

--------

makeNewName :: [Names] -> Names -> Names 
makeNewName xs (y1, y2) = let 
  comped = mapZip (`elem` map snd xs) (newNames y1) :: [(String, Bool)]
  in (y1, fromMaybe y2 (fst <$> find (snd . fmap (/=True)) comped))

newNames :: String -> [String]
newNames s = s : zipWith (++) (repeat s) (map show [0..])

--以下、いきあたりばったり過ぎてクソなのでボツ
--
--alpha :: TypeSentence Names -> [TypeTree Names] -> TypeSentence Names
--alpha typeSentence deduceTree = let
--  sentence = map saveName (nub . allVariables $ concatSentence typeSentence)
--  typeNames = nub . allVariables $ concatTree deduceTree
--  transformed = snd $ runState (transNames typeNames) sentence
--  in foldl (.) id (map (uncurry replaceNameForSentence) transformed) typeSentence
--    where 
--      saveName :: Names -> Names
--      saveName (_, x) = (x, x)
--
--      allVariables :: [TypeTree a] -> [a]
--      allVariables = concatMap tree2Variable 
--     
--      tree2Variable :: TypeTree a -> [a]
--      tree2Variable (TypeVariable x) = [x]
--      tree2Variable _ = []
--
--transNames :: [Names] -> NamesState ()
--transNames [] = return ()
--transNames xs@(next:rest) = do
--  --重複しないよう型変数名を変換
--  modify $ replaceName next (getNewName xs next)
--  --再帰
--  transNames rest
--    where
--      getNewName :: [Names] -> Names -> Names 
--      getNewName xs (y1, y2) = let 
--        comped = mapZip (`elem` map snd xs) (newNames y1) :: [(String, Bool)]
--        in (y1, fromMaybe y2 (fst <$> find (snd . fmap (/=True)) comped))
--
--      newNames :: String -> [String]
--      newNames s = s : zipWith (++) (repeat s) (map show [0..])
--
--replaceNameForSentence :: String -> String -> TypeSentence Names -> TypeSentence Names
--replaceNameForSentence x y (TApply ts) = TApply $ map (replaceNameForSentence x y) ts
--replaceNameForSentence x y (TTree ts) = TTree $ map (fmapOf (nameEq $ toPare x) (fmap $ const y)) ts
--
--replaceName :: Names -> Names -> [Names] -> [Names]
--replaceName x y = replaceFOf (nameEq x) x y --TODO このreplaceFOfはやめた方が良いkgs
--
--nameEq :: Names -> Names -> Bool
--nameEq x y = snd $ (==) <$> x <*> y

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

isTypeVariable :: TypeTree a -> Bool
isTypeVariable (TypeVariable _) = True
isTypeVariable _ = False

allTreeNames :: [TypeTree a] -> [a]
allTreeNames = concatMap tree2List

tree2List :: TypeTree a -> [a]
tree2List (TypeVariable x) = [x]
tree2List (TypeName x) = [x]
tree2List _ = []

--------
-- Utils
--------

toPare :: a -> (a, a)
toPare a = (a, a)

mapZip :: (a -> b) -> [a] -> [(a, b)]
mapZip f = map ((,) <*> f)

fmapOf :: Functor f => (a -> Bool) -> (a -> a) -> f a -> f a 
fmapOf f1 f2 = fmap (\y -> if f1 y then f2 y else y)

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

--------

substitute :: Eq a => a -> a -> a -> a
substitute bef aft = \x -> if x == bef then aft else x

replaceF :: (Eq a, Functor f) => a -> a -> f a -> f a
replaceF bef aft = fmap (substitute bef aft)

replaceF2 :: (Eq a, Functor f, Functor g) => a -> a -> f (g a) -> f (g a)
replaceF2 bef aft = fmap2 (substitute bef aft)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "--------------------------------------"
  print . fmap typeInference =<< parseTypeSentence "{a -> a0 -> b} {c -> a}"
