import Text.Trifecta
import Control.Applicative

data TypeTree 
  = TypeName String
  | TypeValue String
  | SubTree [TypeTree]
  deriving (Show, Read, Eq)

--こういうのなんかありそう
word :: Parser a -> Parser a
word p = spaces >> p >>= \res -> spaces >> return res

typeName :: Parser TypeTree
typeName = TypeName <$> word ((:) <$> upper <*> many letter)

typeValue :: Parser TypeTree
typeValue = TypeValue <$> word ((:) <$> letter <*> many letter)

subTree :: Parser TypeTree
subTree = SubTree <$> word (parens typeSyntax)

typeSyntax :: Parser [TypeTree]
typeSyntax = sepBy (subTree <|> typeName <|> typeValue) (symbol "->")

main :: IO ()
main = do
   parseTest typeSyntax "a -> b -> a"
   parseTest typeSyntax "Hoge -> piyo -> Huga"
   parseTest typeSyntax "(a -> b) -> (b -> c) -> a -> c"
   parseTest typeSyntax "(a -> ) -> (c -> d) "
   parseTest typeSyntax "a ->  -> c"
