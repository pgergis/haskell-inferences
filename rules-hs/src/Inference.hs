{-# LANGUAGE DeriveGeneric #-}

module Inference where

import Data.Aeson
import GHC.Generics

type Type = [Char]

data Rule = Rule
  { premises :: [Type]
  , conclusion :: Type
  } deriving (Eq, Ord, Show, Generic)

instance ToJSON Rule
instance FromJSON Rule

-- this may need updating
type Pretreated = ()

pretreat :: [Rule] -> Pretreated
-- COMPUTE and RETURN Pre-treated data structure
pretreat = undefined

inferoutputs :: Pretreated -> [Type] -> [Type]
-- COMPUTE and RETURH valid inferences
inferoutputs = undefined
