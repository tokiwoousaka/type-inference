import Text.Trifecta
import Control.Applicative

data TypeTree 
  = TypeName String
  | TypeValue String
  | SubTree [TypeTree]
  | Apply [[TypeTree]]
  deriving (Show, Read, Eq)

--こういうのなんかありそう
word :: Parser a -> Parser a
word p = spaces >> p >>= \res -> spaces >> return res

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

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
  = (:[]) . Apply <$> many1 (try $ parens typeApplySentence) <|> typeSyntax

--------

main :: IO ()
main = do
   parseTest (typeApplySentence >>= \res -> eof >> return res) "a -> b -> a"
   parseTest (typeApplySentence >>= \res -> eof >> return res) "a -> b c -> a"
   parseTest (typeApplySentence >>= \res -> eof >> return res) "((a -> b) (a -> c)) (c -> d)"
   parseTest (typeApplySentence >>= \res -> eof >> return res)  "((a -> b) -> (a -> c))(c -> d)" --NG

   --parseTest typeSyntax "a -> b -> a"
   --parseTest typeSyntax "Hoge -> piyo -> Huga"
   --parseTest typeSyntax "(a -> b) -> (b -> c) -> a -> c"
   --parseTest typeSyntax "(a -> ) -> (c -> d) "
   --parseTest typeSyntax "a ->  -> c"
