{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module API.Models where

import Data.Text (Text)
import Data.UUID (UUID)
import Prelude hiding (id)

data Molecule = Molecule
  { id :: Maybe UUID,
    smiles :: Text,
    iupacName :: Text
  }
  deriving (Show, Eq)

data Reaction = Reaction
  { id :: Maybe UUID,
    name :: Text
  }
  deriving (Show, Eq)

data Catalyst = Catalyst
  { id :: Maybe UUID,
    smiles :: Text,
    name :: Maybe Text
  }
  deriving (Show, Eq)

data PRODUCT_FROM = PRODUCT_FROM
  { amount :: Float,
    inputEntity :: Maybe UUID,
    outputEntity :: Maybe UUID
  }
  deriving (Show, Eq)

type REAGENT_IN = PRODUCT_FROM

data ACCELERATE = ACCELERATE
  { temperature :: Float,
    pressure :: Float,
    catalyst :: Maybe UUID,
    reaction :: Maybe UUID
  }
  deriving (Show, Eq)

data MoleculeOrUUID = M Molecule | MU UUID deriving (Show, Eq)

data CatalystOrUUID = C Catalyst | CU UUID deriving (Show, Eq)

data ReactionInput = ReactionInput
  { reaction :: Reaction,
    reagents :: [(REAGENT_IN, MoleculeOrUUID)],
    product :: (PRODUCT_FROM, MoleculeOrUUID),
    catalysts :: [(ACCELERATE, CatalystOrUUID)]
  }
  deriving (Show, Eq)

data PathNode = PathNode {label :: Text, id :: UUID} deriving (Show, Eq)

getCatalystId :: Catalyst -> Maybe UUID
getCatalystId Catalyst {..} = id
