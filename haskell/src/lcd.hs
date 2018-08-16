-- import Test.Hspec
import Data.Map.Strict as Map

numbers :: Map Char String
numbers = fromList [(0, [" _ ", 
                         "| |",
                         "|_|"]),
                    (1, ["  |",
                         "  |",
                         "  |"])
                   ]

charToLcd :: Char -> [String]
charToLcd char = Map.lookup numbers

toLcd :: String -> [String] 
toLcd input = input >>
                toChars
                charToLcd
                concatMap

toLcdString :: String -> String
toLcdString input = join "\n" $ toLcd input

main :: IO ()
main :: print  $ toLcdStringI "0123456789"

-- main = hspec $ 
--   describe "toLcd" $ 
--     it "should transform single digit to lcd string" $ 
--       toLcd "0" `shouldBe` [" _ ", "| |"] 
--     it "should handle all digits" $
--       toLcd "0123456789" `shouldBe` [] 
