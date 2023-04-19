{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module API.Models where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Prelude hiding (id)

data Molecule = Molecule
  { id :: Maybe UUID,
    smiles :: Text,
    iupacName :: Text
  }
  deriving (Generic, Show, Eq)

instance ToSchema Molecule

instance FromJSON Molecule

data Reaction = Reaction
  { id :: Maybe UUID,
    name :: Text
  }
  deriving (Generic, Show, Eq)

instance ToSchema Reaction

instance FromJSON Reaction

instance ToJSON Reaction

data Catalyst = Catalyst
  { id :: Maybe UUID,
    smiles :: Text,
    name :: Maybe Text
  }
  deriving (Generic, Show, Eq)

instance ToSchema Catalyst

instance FromJSON Catalyst

data PRODUCT_FROM = PRODUCT_FROM
  { amount :: Float,
    inputEntity :: Maybe UUID,
    outputEntity :: Maybe UUID
  }
  deriving (Generic, Show, Eq)

instance ToSchema PRODUCT_FROM

instance FromJSON PRODUCT_FROM

type REAGENT_IN = PRODUCT_FROM

data ACCELERATE = ACCELERATE
  { temperature :: Float,
    pressure :: Float,
    catalyst :: Maybe UUID,
    reaction :: Maybe UUID
  }
  deriving (Generic, Show, Eq)

instance ToSchema ACCELERATE

instance FromJSON ACCELERATE

data MoleculeOrUUID = M Molecule | MU UUID deriving (Generic, Show, Eq)

instance FromJSON MoleculeOrUUID

instance ToSchema MoleculeOrUUID

data CatalystOrUUID = C Catalyst | CU UUID deriving (Generic, Show, Eq)

instance FromJSON CatalystOrUUID

instance ToSchema CatalystOrUUID

data ReactionInput = ReactionInput
  { reaction :: Reaction,
    reagents :: [(REAGENT_IN, MoleculeOrUUID)],
    product :: (PRODUCT_FROM, MoleculeOrUUID),
    catalysts :: [(ACCELERATE, CatalystOrUUID)]
  }
  deriving (Generic, Show, Eq)

instance ToSchema ReactionInput

instance FromJSON ReactionInput

data PathNode = PathNode {label :: Text, id :: UUID} deriving (Show, Eq)

getCatalystId :: Catalyst -> Maybe UUID
getCatalystId Catalyst {..} = id
