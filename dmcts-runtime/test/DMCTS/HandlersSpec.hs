module DMCTS.HandlersSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "Client" $ do
    it "Is a dummy test" $ do
      1 `shouldBe` 1
