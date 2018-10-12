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

type Pretreated = [(Set.Set Type, Type)]

pretreat :: [Rule] -> Pretreated
pretreat rules = pretreatSorter [(Set.fromList (premises rule), conclusion rule) | rule <- rules]

pretreatSorter :: Pretreated -> Pretreated
pretreatSorter [] = []
pretreatSorter (rule@(prems, conc):rules) =
  let prereqs = pretreatSorter [(p,c) | (p,c) <- rules, Set.member c prems]
      others = pretreatSorter [(p,c) | (p,c) <- rules, Set.notMember c prems]
  in others ++ [rule] ++ prereqs

inferoutputs :: Pretreated -> [Type] -> [Type]
inferoutputs rules assertions = Set.elems $ inferoutputsHelper rules (Set.fromList assertions)

inferoutputsHelper :: Pretreated -> Set.Set Type -> Set.Set Type
inferoutputsHelper rules assertions = Set.difference (Prelude.foldr infer assertions rules) assertions
  where infer (prems,conc) knowledge =
          if Set.isSubsetOf prems knowledge
          then Set.insert conc knowledge
          else knowledge
