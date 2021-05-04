module DMCTSKnapsack.KnapTreeSpec where

import Test.Hspec

import DMCTSKnapsack.KnapTree
import DMCTS.Types
import ExampleTrees


spec :: Spec
spec = do
  describe "KnapTree" $ do
    describe "pickItemNR" $ do
      it "should subtract the choice from remSpace" $ do
        remSpace (pickItemNR exNoSpace 0) `shouldBe` -1
      it "should remove an element from the opt list" $ do
        opt (pickItemNR exNoSpace 0) `shouldBe` [2,3,4]

    describe "pickItemWR" $ do
      it "should subtract the choice from remSpace" $ do
        remSpace (pickItemWR exNoSpace 0) `shouldBe` -1
      it "shouldn't touch the opt list" $ do
        opt (pickItemWR exNoSpace 0) `shouldBe` opt exNoSpace

    describe "KnapLogicNR" $ do
      it "shouldn't return any children if we have no opt" $ do
        children KnapLogicNR exNoOpt `shouldBe` []
      it "shouldn't return any children if we have no space" $ do
        children KnapLogicNR exNoSpace `shouldBe` []

    describe "KnapLogicWR" $ do
      it "shouldn't return any children if we have no opt" $ do
        children KnapLogicWR exNoOpt `shouldBe` []
      it "shouldn't return any children if we have no space" $ do
        children KnapLogicWR exNoSpace `shouldBe` []
