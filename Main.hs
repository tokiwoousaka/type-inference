import Text.Trifecta
import Control.Applicative

data TypeTree 
  = TypeName String
  | TypeValue String
  | SubTree [TypeTree]
  | Apply [[TypeTree]]
  deriving (Show, Read, Eq)

--------

--こういうのなんかありそう
word :: Parser a -> Parser a
word p = spaces >> p >>= \res -> spaces >> return res

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

--------

typeName :: Parser TypeTree
typeName = TypeName <$> word ((:) <$> upper <*> many letter)

typeValue :: Parser TypeTree
typeValue = TypeValue <$> word (many1 letter)

subTree :: Parser TypeTree
subTree = SubTree <$> word (parens typeSyntax)

typeSyntax :: Parser [TypeTree]
typeSyntax = sepBy (subTree <|> typeName <|> typeValue) (symbol "->")

typeApplySentence :: Parser [TypeTree]
typeApplySentence 
  = (:[]) . Apply <$> many1 (braces typeApplySentence) <|> typeSyntax

parseTypeSentence :: String -> [TypeTree]
parseTypeSentence = undefined

--------

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
     where
       t = parseTest (typeApplySentence >>= \res -> eof >> return res) 
