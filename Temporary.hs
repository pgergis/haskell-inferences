import Data.Set

type Facts = Set Char
type Rule = (Facts,Facts)

pretreat :: [([Char],[Char])] -> [Rule]
pretreat rules = pretreatHelper [(fromList premises, fromList conclusions) | (premises,conclusions) <- rules]

inferoutputs :: [Rule] -> [Char] -> Facts
inferoutputs rules assertions = inferoutputsHelper rules (fromList assertions)



-- quicksort-like algorithm for arranging rules in reverse order of dependencies
-- reversed so we can use a foldr instead of foldl
pretreatHelper :: [Rule] -> [Rule]
pretreatHelper [] = []
pretreatHelper (rule:[]) = [rule]
pretreatHelper (rule@(premises,conclusions):rules) =
  let prereqs = pretreatHelper [(p,c) | (p,c) <- rules, isSubsetOf c premises]
      dependencies = pretreatHelper [(p,c) | (p,c) <- rules, not $ isSubsetOf c premises]
  in dependencies ++ [rule] ++ prereqs

-- builds knowledge set by starting with assertions and building with valid conclusions
-- a valid conclusion is one where the premises exist within our knowledge set
inferoutputsHelper :: [Rule] -> Facts -> Facts
inferoutputsHelper rules assertions = difference (Data.Set.foldr infer assertions (fromList rules)) assertions
  where infer (p,c) k =
          if isSubsetOf p k
          then Data.Set.union c k
          else k
