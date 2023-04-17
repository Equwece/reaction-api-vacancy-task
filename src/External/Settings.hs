module External.Settings where
import Data.Text (Text)

data Settings = Settings
  { neo4j_host :: String,
    neo4j_port :: Int,
    neo4j_user :: Text,
    neo4j_pass :: Text
  }
  deriving (Show, Eq)
