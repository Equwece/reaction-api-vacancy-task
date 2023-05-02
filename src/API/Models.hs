{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module API.Models where

import Control.Arrow (ArrowChoice (left))
import Data.Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Functor ((<&>))
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8')
import Data.UUID (UUID)
import GHC.Generics (Generic)
import GHC.TypeLits (KnownSymbol)
import Servant
import Servant.HTML.Blaze (HTML)
import Servant.Swagger.UI.Core (SwaggerUiHtml (..))
import Prelude hiding (id)

data Molecule = Molecule
  { id :: Maybe UUID,
    smiles :: Text,
    iupacName :: Text
  }
  deriving (Generic, Show, Eq)

instance ToSchema Molecule

instance FromJSON Molecule

instance ToJSON Molecule

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

instance ToJSON Catalyst

newtype PRODUCT_FROM = PRODUCT_FROM
  { amount :: Float
  }
  deriving (Generic, Show, Eq)

instance ToSchema PRODUCT_FROM

instance FromJSON PRODUCT_FROM

instance ToJSON PRODUCT_FROM

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

instance ToJSON ACCELERATE

data MoleculeOrUUID = M Molecule | MU UUID deriving (Generic, Show, Eq)

instance FromJSON MoleculeOrUUID

instance ToJSON MoleculeOrUUID

instance ToSchema MoleculeOrUUID

data CatalystOrUUID = C Catalyst | CU UUID deriving (Generic, Show, Eq)

instance FromJSON CatalystOrUUID

instance ToJSON CatalystOrUUID

instance ToSchema CatalystOrUUID

data ReactionInput = ReactionInput
  { reaction :: Reaction,
    reagents :: [ReagentInput],
    product :: ProductInput,
    catalysts :: [CatalystInput]
  }
  deriving (Generic, Show, Eq)

instance ToSchema ReactionInput

instance FromJSON ReactionInput

instance ToJSON ReactionInput

type ReagentInput = ProductInput

data ProductInput = ProductInput
  { relation :: PRODUCT_FROM,
    molecule :: MoleculeOrUUID
  }
  deriving (Generic, Show, Eq)

instance ToSchema ProductInput

instance FromJSON ProductInput

instance ToJSON ProductInput

data CatalystInput = CatalystInput
  { accelerate :: ACCELERATE,
    catalyst :: CatalystOrUUID
  }
  deriving (Generic, Show, Eq)

instance ToSchema CatalystInput

instance FromJSON CatalystInput

instance ToJSON CatalystInput

data PathNode = PathNode {label :: Text, id :: UUID} deriving (Generic, Show, Eq)

instance ToSchema PathNode

instance ToJSON PathNode

instance FromJSON PathNode

getCatalystId :: Catalyst -> Maybe UUID
getCatalystId Catalyst {..} = id

-- An instance is needed for testing via servant-client
instance (KnownSymbol dir, HasLink api, Link ~ MkLink api Link, IsElem api api) => MimeUnrender HTML (SwaggerUiHtml dir api) where
  mimeUnrender _ = left show . eitherText
    where
      eitherText byteStr = (decodeUtf8' . toStrict $ byteStr) <&> SwaggerUiHtml
