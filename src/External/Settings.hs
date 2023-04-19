module External.Settings where

import Data.Text (Text)

data Settings = Settings
  { neo4jHost :: String,
    neo4jPort :: Int,
    neo4jUser :: Text,
    neo4jPass :: Text,
    appPort :: Int
  }
  deriving (Show, Eq)
