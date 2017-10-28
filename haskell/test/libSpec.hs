import Test.Hspec

main = hspec $ do
  describe "This is my math test" $ do
    it "adds two natural numbers" $ do
      (1 + 1) `shouldBe` 2
