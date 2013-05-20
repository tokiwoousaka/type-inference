module Test where
import Test.QuickCheck
import qualified Main as M

prop_MakeNewName :: [M.Names] -> M.Names -> Bool
prop_MakeNewName ns n = named (map snd ns) (snd $ M.makeNewName ns n)

named :: [String] -> String -> Bool
named xs x = and $ zipWith (/=) xs (repeat $ x)

main = do
  verboseCheck prop_MakeNewName
