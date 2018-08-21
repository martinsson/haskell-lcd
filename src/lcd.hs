module Lcd (toLcdString, toLcdResult) where 

import Data.List as List
import Data.Map.Strict as Map
import Data.Maybe as Maybe
import Data.Either

toLcdResult :: String -> Either String String
toLcdResult input = let nonLcdChars = List.filter hasLcdChar input in
                        toLcdHelper nonLcdChars input where
                          
                          toLcdHelper :: [Char] -> String -> Either String String
                          toLcdHelper [] input = Right (toLcdString input)
                          toLcdHelper nonLcds _ = Left ("'" ++ (intersperse ',' nonLcds) ++  "' don't have a LCD representation") 

                          hasLcdChar c = isNothing (Map.lookup c lcdDigits) 

toLcdString :: String -> String
toLcdString = show . mconcat . toLcdDigits

newtype LcdString = LcdString [String]

instance Monoid LcdString where
  mempty = LcdString ["", "", ""]
  mappend (LcdString linesFst) (LcdString linesSnd) = LcdString $ zipWith (++) linesFst linesSnd

instance Show LcdString where
  show (LcdString lines) = intercalate "\n" lines


toLcdDigits :: String -> [LcdString]
toLcdDigits = Maybe.mapMaybe getLcdDigit where
                getLcdDigit char = Map.lookup char lcdDigits

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
                             " _|"])]
                     
