module Spec where

import Test.Hspec
import Inference
import qualified Data.Set as Set

isSyllogisticallySorted :: Pretreated -> Bool
isSyllogisticallySorted [] = True
isSyllogisticallySorted (r:[]) = True
isSyllogisticallySorted rules@(r:rest) = and [(premisesNotInFutureConclusions r rest), (isSyllogisticallySorted rest)]

premisesNotInFutureConclusions :: Syllogism -> [Syllogism] -> Bool
premisesNotInFutureConclusions r rs = Set.disjoint (fst r) (Set.fromList $ snd $ unzip rs)

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
    fail1 = Rule {premises = ["E"], conclusion = "C"}
    fail2 = Rule {premises = ["G"], conclusion = "E"}
    fail3 = Rule {premises = ["H"], conclusion = "G"}
    fail4 = Rule {premises = ["J"], conclusion = "C"}
    failing = [fail1,
               fail2,
               fail3]
  in
    describe "Inference" $ do
      describe "pretreat" $ do
        it "returns empty list if no rules are given" $
          pretreat [] `shouldBe` []

        it "returns the type-modified rule if one rule is given" $
          pretreat [rule1] `shouldBe` [(Set.fromList p1, c1)]

        it "returns list where no premises appear in proceeding conclusions (1 of 6)" $ do
          pretreat [rule1,rule3,rule2] `shouldBe` [(Set.fromList p3, c3), (Set.fromList p2, c2), (Set.fromList p1, c1)]
        it "returns list where no premises appear in proceeding conclusions (2 of 6)" $ do
          pretreat [rule1,rule2,rule3] `shouldBe` [(Set.fromList p3, c3), (Set.fromList p2, c2), (Set.fromList p1, c1)]
        it "returns list where no premises appear in proceeding conclusions (3 of 6)" $ do
          pretreat [rule2,rule1,rule3] `shouldBe` [(Set.fromList p3, c3), (Set.fromList p2, c2), (Set.fromList p1, c1)]
        it "returns list where no premises appear in proceeding conclusions (4 of 6)" $ do
          pretreat [rule2,rule3,rule1] `shouldBe` [(Set.fromList p3, c3), (Set.fromList p2, c2), (Set.fromList p1, c1)]
        it "returns list where no premises appear in proceeding conclusions (5 of 6)" $ do
          pretreat [rule3,rule1,rule2] `shouldBe` [(Set.fromList p3, c3), (Set.fromList p2, c2), (Set.fromList p1, c1)]
        it "returns list where no premises appear in proceeding conclusions (6 of 6)" $ do
          pretreat [rule3,rule2,rule1] `shouldBe` [(Set.fromList p3, c3), (Set.fromList p2, c2), (Set.fromList p1, c1)]
        it "handles failing test cases (1 of 6)" $ do
          pretreat failing `shouldBe` reverse [(Set.fromList ["H"], "G"), (Set.fromList ["G"], "E"), (Set.fromList ["E"], "C")]
        it "handles failing test cases (2 of 6)" $ do
          pretreat [fail1,fail2,fail3] `shouldBe` reverse [(Set.fromList ["H"], "G"), (Set.fromList ["G"], "E"), (Set.fromList ["E"], "C")]
        it "handles failing test cases (3 of 6)" $ do
          pretreat [fail1,fail3,fail2] `shouldBe` reverse [(Set.fromList ["H"], "G"), (Set.fromList ["G"], "E"), (Set.fromList ["E"], "C")]
        it "handles failing test cases (4 of 6)" $ do
          pretreat [fail2,fail1,fail3] `shouldBe` reverse [(Set.fromList ["H"], "G"), (Set.fromList ["G"], "E"), (Set.fromList ["E"], "C")]
        it "handles failing test cases (5 of 6)" $ do
          pretreat [fail2,fail3,fail1] `shouldBe` reverse [(Set.fromList ["H"], "G"), (Set.fromList ["G"], "E"), (Set.fromList ["E"], "C")]
        it "handles failing test cases (6 of 6)" $ do
          pretreat failing `shouldBe` reverse [(Set.fromList ["H"], "G"), (Set.fromList ["G"], "E"), (Set.fromList ["E"], "C")]

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
          inferoutputs (pretreat [rule1, rule3]) (p1++p3) `shouldBe` [c1,c3]

        it "infers Rule 1 conclusion but not Rule 3 conclusion if only Rule 1 premises asserted" $ do
          inferoutputs (pretreat [rule1,rule3]) p1 `shouldBe` [c1]

        it "infers all conclusions from chain of conclusions -> premises" $ do
          inferoutputs (pretreat [rule1,rule2,rule3]) (p1++["d","f"]) `shouldBe` [c1,c2,c3]

      describe "failing test" $ do
        it "this ruleset fails to produce the correct output" $ do
          inferoutputs (pretreat failing) (["H"]) `shouldBe` ["C", "E", "G"]

        it "handles multiple premises to same conclusion" $ do
          inferoutputs (pretreat [fail1,fail2,fail3,fail4]) (["H"]) `shouldBe` ["C", "E", "G"]

        it "handles disjointed premises" $ do
          inferoutputs (pretreat [fail2,fail3,fail4]) (["H"]) `shouldBe` ["E", "G"]
