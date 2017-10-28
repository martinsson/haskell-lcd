import Test.Hspec

newtype Amount = Amount { value :: Int }
	deriving (Show, Eq)

main = hspec $ do
  describe "bankaccount" $ do
    it "new account should be 0" $ do
      add (Amount 60) (Amount 40) `shouldBe` (Amount 100)


add :: Amount -> Amount -> Amount 
add (Amount x) (Amount y) = Amount (x + y)  



