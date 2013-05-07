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
typeValue = TypeValue <$> word (many letter)

typeSyntax :: Parser [TypeTree]
typeSyntax = sepBy (SubTree <$> parens typeSyntax <|> typeName <|> typeValue) (symbol "->")

main :: IO ()
main = do
   parseTest typeSyntax "a -> b -> a"
   parseTest typeSyntax "Hoge -> piyo -> Huga"
   parseTest typeSyntax "(a -> b) -> (b -> c) -> a -> c"

