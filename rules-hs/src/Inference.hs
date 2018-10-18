{-# LANGUAGE DeriveGeneric #-}

module Inference where

import Data.Aeson
import GHC.Generics
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

pretreatSort :: [Syllogism] -> Pretreated
pretreatSort rules = dfsSort rules Set.empty []

dfsSort :: [Syllogism] -> Set.Set Syllogism -> [Syllogism] -> Pretreated
dfsSort graph visited stack =
  let
      visited' = Set.union visited (Set.fromList (stack))
      unvisited = filter (\a -> Set.notMember a visited') graph
  in
    if unvisited == []
    then stack
    else
      let
        newdfs = dfs (head unvisited) graph Set.empty
        stack' = filter (\a -> Set.notMember a (Set.fromList newdfs)) stack
      in dfsSort graph visited' $ stack' ++ newdfs

dfs :: Syllogism -> [Syllogism] -> Set.Set Syllogism -> [Syllogism]
dfs node@(_,curConc) graph visited =
  let
    unvisited = [n | n <- graph, Set.notMember n visited]
    children = [c | c@(childPrems,_) <- unvisited, Set.member curConc childPrems]
    next = children
    visited' = Set.union visited (Set.fromList ([node] ++ next))
  in
    if next == []
    then [node]
    else [node] ++ (dfs (head next) graph visited')

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
