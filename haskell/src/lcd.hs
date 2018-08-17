-- import Test.Hspec
import Data.List
import Data.Map.Strict as Map

lcdDigits :: Map Char [String]
lcdDigits = fromList [('0', [" _ ", 
                           "| |",
                           "|_|"]),
                    ('1', ["   ",
                           "  |",
                           "  |"]),
                    ('2', [" _ ",
                           " _|",
                           "|_ "]),
                    ('3', [" _ ",
                           " _|",
                           " _|"]),
                    ('4', ["   ",
                           "|_|",
                           "  |"]),
                    ('5', [" _ ",
                           "|_ ",
                           " _|"]),
                    ('6', [" _ ",
                           "|_ ",
                           "|_|"]),
                    ('7', [" _ ",
                           "  |",
                           "  |"]),
                    ('8', [" _ ",
                           "|_|",
                           "|_|"]),
                    ('9', [" _ ",
                           "|_|",
                           " _|"])
                   ]

newtype LcdString = LcdString [String]

instance Monoid LcdString where
  mempty = LcdString ["", "", ""]
  mappend (LcdString linesFst) (LcdString linesSnd) = LcdString $ zipWith (++) linesFst linesSnd

instance Show LcdString where
  show (LcdString lines) = intercalate "\n" lines

charToLcd :: Char -> LcdString 
charToLcd char = LcdString $ Map.findWithDefault emptyLines char lcdDigits
                  where emptyLines = ["", "", ""]

toLcd :: String -> LcdString
toLcd input = mconcat $ Prelude.map charToLcd  input

toLcdString :: String -> String
toLcdString = show . toLcd

main :: IO ()
main = putStrLn (toLcdString "0123456789")
