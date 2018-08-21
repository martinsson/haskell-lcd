import Test.Hspec
import Lcd
import Data.Either

main = hspec $
  describe "toLcdString" $ do
    it "should transform single digit to lcd string" $
      toLcdString "0" `shouldBe` " _ " ++ "\n" ++ 
                                 "| |" ++ "\n" ++  
                                 "|_|" 
    it "should handle all digits" $
      toLcdString "0123456789" `shouldBe` " _     _  _     _  _  _  _  _ \n" ++
                                          "| |  | _| _||_||_ |_   ||_||_|\n" ++
                                          "|_|  ||_  _|  | _||_|  ||_| _|" 
 
    it "should validate input" $ 
      toLcdResult "1ab" `shouldBe` (Left "'a,b' don't have a LCD representation")
    it "should validate input" $ 
      toLcdResult "12" `shouldSatisfy` isRight
