-- import Test.Hspec
import Data.List
import Data.Map.Strict as Map

numbers :: Map Char [String]
numbers = fromList [('0', [" _ ", 
                           "| |",
                           "|_|"]),
                    ('1', ["  |",
                           "  |",
                           "  |"])
                   ]

newtype LcdString = LcdString [String]

instance Monoid LcdString where
  mempty = LcdString ["", "", ""]
  mappend (LcdString a) (LcdString b) = LcdString $ zipWith (++) a b

charToLcd :: Char -> LcdString 
charToLcd char = LcdString $ Map.findWithDefault zeroLcd char numbers
                  where zeroLcd = ["", "", ""]

toLcd :: String -> LcdString
toLcd input = mconcat $ Prelude.map charToLcd  input

toLcdString :: String -> String
toLcdString input = let (LcdString lines) = toLcd input in
			intercalate "\n" lines

main :: IO ()
main = putStrLn (toLcdString "01")
