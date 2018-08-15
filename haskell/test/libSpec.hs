import Test.Hspec

newtype Amount = Amount { value :: Int }
  deriving (Show, Eq)

main = hspec $
  describe "bankaccount" $ do
    it "new account should be 0" $
      add (Amount 60) (Amount 40) `shouldBe` (Amount 100)
    it "balance of new account is 0" $
      balance NewAccount `shouldBe` Amount 0


add :: Amount -> Amount -> Amount 
add (Amount x) (Amount y) = Amount (x + y)  

data Account = NewAccount

balance :: Account -> Amount
balance _ = Amount 0


