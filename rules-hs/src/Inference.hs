{-# LANGUAGE DeriveGeneric #-}

module Inference where

-- import Debug.Trace

import Data.Aeson
import GHC.Generics
import Data.List
import qualified Data.Set as Set

type Type = [Char]

data Rule = Rule
  { premises :: [Type]
  , conclusion :: Type
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON Rule
instance FromJSON Rule

type Pretreated = [Syllogism]
type Premises = Set.Set Type
type Conclusion = Type
type Syllogism = (Premises, Conclusion)

-----------------
-- Pretreating --
-----------------

pretreat :: [Rule] -> Pretreated
pretreat rules = reverse $ pretreatSort [(Set.fromList (premises rule), conclusion rule) | rule <- rules]

dependentOn :: Syllogism -> Syllogism -> Bool
s1 `dependentOn` s2 = Set.member (snd s2) (fst s1)

pretreatSort :: [Syllogism] -> Pretreated
pretreatSort [] = []
pretreatSort (r:[]) = [r]
pretreatSort (r1:r2:[]) = if r1 `dependentOn` r2 then [r2,r1] else [r1,r2]
pretreatSort rules@(r:rest) =
  if isSyllogisticallySorted rules
  then rules
  else
    -- trace ("sorting: " ++ show rules) $
    -- pretreatSort $ sortBy compareSyllogisms (rest++[r])
    dfsSort r rules [r] (Set.singleton r)

dfsSort :: Syllogism -> [Syllogism] -> Pretreated -> Set.Set Syllogism -> Pretreated
dfsSort start graph path visited =
  -- trace ("start: " ++ show start ++ "\nnext: " ++ show next ++ "\npath: " ++ show path) $
  let
    filterUnvisited = filter (\a -> Set.notMember a visited)
    next = filterUnvisited $ filter ((flip dependentOn) start) graph
    visited' = Set.union visited (Set.fromList next)
  in
    -- trace ("new next: " ++ show next' ++ "\nunvisited: " ++ (show $ filterUnvisited graph)) $
    if next == []
    then
      let unvisited = filterUnvisited graph in
        if unvisited == []
        then path
        else pretreatSort (unvisited++path)
    else concat $ map (\node -> dfsSort node graph (path++[node]) visited') next

compareSyllogisms :: Syllogism -> Syllogism -> Ordering
compareSyllogisms s1 s2
  | Set.member (snd s1) (fst s2) = GT
  | Set.member (snd s2) (fst s1) = LT
  | otherwise = EQ

isSyllogisticallySorted :: Pretreated -> Bool
isSyllogisticallySorted [] = True
isSyllogisticallySorted (r:[]) = True
isSyllogisticallySorted rules@(r:rest) = and [(premisesNotInFutureConclusions r rest), (isSyllogisticallySorted rest)]

premisesNotInFutureConclusions :: Syllogism -> [Syllogism] -> Bool
premisesNotInFutureConclusions r rs = Set.disjoint (fst r) (Set.fromList $ snd $ unzip rs)

---------------
-- Inference --
---------------

inferoutputs :: Pretreated -> [Type] -> [Type]
inferoutputs rules assertions = Set.elems $ inferoutputsHelper rules (Set.fromList assertions)

inferoutputsHelper :: Pretreated -> Set.Set Type -> Set.Set Type
inferoutputsHelper rules assertions = Set.difference (Prelude.foldr infer assertions rules) assertions
  where infer (prems,conc) knowledge =
          if prems `containedIn` knowledge
          then Set.insert conc knowledge
          else knowledge
        containedIn p k =
          and $ Set.map ((flip Set.member) k) p
