module Spec where

import Test.Hspec
import Inference
import qualified Data.Set as Set

main :: IO ()
main = hspec $
  let
    p1 = ["a","b"]
    c1 = "c"
    rule1 = Rule {premises = p1, conclusion = c1}
    p2 = ["c","d"]
    c2 = "e"
    rule2 = Rule {premises = p2, conclusion = c2}
    p3 = ["e","f"]
    c3 = "g"
    rule3 = Rule {premises = p3, conclusion = c3}
  in
    describe "Inference" $ do
      describe "pretreat" $ do
        it "returns empty list if no rules are given" $
          pretreat [] `shouldBe` []

        it "returns the type-modified rule if one rule is given" $
          pretreat [rule1] `shouldBe` [(Set.fromList p1, c1)]

        it "reverse sorts rules by premise dependency" $ do
          pretreat [rule1,rule2] `shouldBe` [(Set.fromList p2, c2), (Set.fromList p1, c1)]

      describe "inferoutputs" $ do
        it "infers nothing from rules if nothing is asserted" $ do
          inferoutputs (pretreat [rule1]) [] `shouldBe` []

        it "infers nothing from assertions if no rules are given" $ do
          inferoutputs (pretreat []) p1 `shouldBe` []

        it "infers nothing if no rules and no assertions are given" $ do
          inferoutputs (pretreat []) [] `shouldBe` []

        it "infers conclusion when premises are asserted" $ do
          inferoutputs (pretreat [rule1]) p1 `shouldBe` [c1]

        it "infers all conclusions when all rule premises are asserted" $ do
          inferoutputs (pretreat [rule1, rule3]) p1++p3 `shouldBe` [c1,c3]

        it "infers Rule 1 conclusion but not Rule 3 conclusion if Rule 1 premises asserted" $ do
          inferoutputs (pretreat [rule1,rule3]) p1 `shouldBe` [c1]

        it "infers all conclusions from chain of conclusions -> premises" $ do
          inferoutputs (pretreat [rule1,rule2,rule3]) p1++["d","f"] `shouldBe` [c1,c2,c3]
