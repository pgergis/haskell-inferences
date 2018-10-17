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
type Syllogism = (Set.Set Type, Type)

pretreat :: [Rule] -> Pretreated
pretreat rules = reverse $ dfs [(Set.fromList (premises rule), conclusion rule) | rule <- rules]

dependentOn :: Syllogism -> Syllogism -> Bool
r1 `dependentOn` r2 = Set.member (snd r2) (fst r1)

dfs :: [Syllogism] -> Pretreated
dfs [] = []
dfs (r:[]) = [r]
dfs (r1:r2:[]) = if r1 `dependentOn` r2 then [r2,r1] else [r1,r2]
dfs (r:rules) = dfs' r rules [r] Set.empty

dfs' :: Syllogism -> [Syllogism] -> Pretreated -> Set.Set Syllogism -> Pretreated
dfs' start graph path visited =
  let
    filterUnvisited = filter (\a -> Set.notMember a visited)
    next = filterUnvisited $ filter ((flip dependentOn) start) graph
    visited' = Set.union visited (Set.fromList next)
  in
    -- trace (show start ++ " | " ++ show graph ++ " | " ++ show path ++ " | " ++ show visited) $
    if next == []
    then
      let unvisited = filterUnvisited graph in
        if unvisited == []
        then path
        else dfs (unvisited++path)
    else concat $ map (\node -> dfs' node graph (path++[node]) visited') next

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
