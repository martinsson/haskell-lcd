import Test.Hspec
import Lcd

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
