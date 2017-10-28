import Test.Hspec

main = hspec $ do 
  describe "BonulMalus"$ do
    it "new drivers pay 100% of the fee" $
      bonus New `shouldBe` B 100

data History = New
  deriving (Eq, Show)

data Bonus = B Int 
  deriving (Eq, Show)

bonus :: History -> Bonus
bonus = undefined

