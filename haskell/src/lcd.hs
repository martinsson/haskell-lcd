-- import Test.Hspec
import Data.Map.Strict as Map

numbers :: Map Char [String]
numbers = fromList [('0', [" _ ", 
                           "| |",
                           "|_|"]),
                    ('1', ["  |",
                           "  |",
                           "  |"])
                   ]

charToLcd :: Char -> [String]
charToLcd char = Map.findWithDefault zeroLcd char numbers
                  where zeroLcd = ["", "", ""]

toLcd :: String -> [String] 
toLcd input = zipWith (++) $ Prelude.map charToLcd  input

toLcdString :: String -> String
toLcdString input = join "\n" $ toLcd input

main :: IO ()
main = print ( toLcdString "0123456789")

