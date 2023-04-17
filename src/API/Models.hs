{-# LANGUAGE DuplicateRecordFields #-}

module API.Models where

import Data.UUID (UUID)

data Molecule = Molecule
  { id :: Maybe UUID,
    smiles :: String,
    iupacName :: String
  }
  deriving (Show, Eq)

data Reaction = Reaction
  { id :: Maybe UUID,
    name :: String
  }
  deriving (Show, Eq)

data Catalyst = Catalyst
  { id :: Maybe UUID,
    smiles :: String,
    names :: Maybe String
  }
  deriving (Show, Eq)

newtype PRODUCT_FROM = PRODUCT_FROM
  { amount :: Float
  }
  deriving (Show, Eq)

data ACCELERATE = ACCELERATE 
  { temperature :: Float,
    pressure :: Float
  }
  deriving (Show, Eq)
