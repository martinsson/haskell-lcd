-- import Test.Hspec
import Data.List
import Data.Map.Strict as Map
import Data.Maybe as Maybe

lcdDigits :: Map Char LcdString
lcdDigits = fromList [('0', LcdString
                             [" _ ", 
                              "| |",
                              "|_|"]),
                    ('1', LcdString
                          ["   ",
                           "  |",
                           "  |"]),
                    ('2', LcdString
                          [" _ ",
                           " _|",
                           "|_ "]),
                    ('3', LcdString
                          [" _ ",
                           " _|",
                           " _|"]),
                    ('4', LcdString
                          ["   ",
                           "|_|",
                           "  |"]),
                    ('5', LcdString
                          [" _ ",
                           "|_ ",
                           " _|"]),
                    ('6', LcdString
                          [" _ ",
                           "|_ ",
                           "|_|"]),
                    ('7', LcdString
                          [" _ ",
                           "  |",
                           "  |"]),
                    ('8', LcdString
                          [" _ ",
                           "|_|",
                           "|_|"]),
                    ('9', LcdString
                          [" _ ",
                           "|_|",
                           " _|"])
                   ]

newtype LcdString = LcdString [String]

instance Monoid LcdString where
  mempty = LcdString ["", "", ""]
  mappend (LcdString linesFst) (LcdString linesSnd) = LcdString $ zipWith (++) linesFst linesSnd

instance Show LcdString where
  show (LcdString lines) = intercalate "\n" lines

toLcd :: String -> LcdString
toLcd input = mconcat $ Maybe.mapMaybe getLcdDigit input where
                getLcdDigit :: Char -> Maybe LcdString
                getLcdDigit char = Map.lookup char lcdDigits


toLcdString :: String -> String
toLcdString = show . toLcd

main :: IO ()
main = putStrLn (toLcdString "0123456789")
