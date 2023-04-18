{-# LANGUAGE RecordWildCards #-}

module Main where

import API.Models (ACCELERATE (ACCELERATE, catalyst, pressure, reaction, temperature), Catalyst (Catalyst, id, name, smiles), CatalystOrUUID (C), Molecule (Molecule, id, iupacName, smiles), MoleculeOrUUID (M), PRODUCT_FROM (PRODUCT_FROM, amount, inputEntity, outputEntity), REAGENT_IN, Reaction (Reaction, id, name), ReactionInput (ReactionInput, catalysts, product, reaction, reagents))
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (forM_, (>=>))
import Control.Monad.Except ()
import Data.Default (Default (def))
import Data.Text (pack)
import qualified Data.Text as T
import Data.Time (diffUTCTime, getCurrentTime)
import Data.UUID (fromString)
import Database.Bolt (BoltCfg (host, password, port, user), connect)
import External.Interfaces (AppEnvironment (..), Logger (Logger, logMsg), Neo4jConn (createReaction, getPath, getReactionNodeById))
import External.Neo4j (Neo4jDB (Neo4jDB))
import External.Settings (Settings (..))
import Script (setupDB)
import System.Environment (getEnv)
import System.Log.FastLogger (LogStr, LogType' (LogStdout), ToLogStr (toLogStr), defaultBufSize, withFastLogger)

main :: IO ()
main = do
  withFastLogger (LogStdout defaultBufSize) $ \fastLogger -> do
    settings <- loadSettings
    let neo4jConnCfg =
          def
            { user = neo4j_user settings,
              password = neo4j_pass settings,
              host = neo4j_host settings,
              port = neo4j_port settings
            }

    db <- Neo4jDB <$> connect neo4jConnCfg
    let logger = Logger {logMsg = wrapLogMsg >=> fastLogger}
        appEnv = AppEnvironment {..}
    logMsg logger "App has started..."
    testScript appEnv

testScript :: Neo4jConn a => AppEnvironment a -> IO ()
testScript appEnv@AppEnvironment {..} = do
  let react1 =
        ReactionInput
          { reaction = Reaction {name = pack "react1", id = fromString "b86943c9-264d-4181-bda7-4830fd650527"},
            reagents =
              [ ( PRODUCT_FROM {amount = 50.3, inputEntity = Nothing, outputEntity = Nothing},
                  M (Molecule {id = Nothing, smiles = pack "mol1_smile", iupacName = pack "mol1_iupac"})
                ),
                ( PRODUCT_FROM {amount = 40.8, inputEntity = Nothing, outputEntity = Nothing},
                  M (Molecule {id = Nothing, smiles = pack "mol2_smile", iupacName = pack "mol2_iupac"})
                )
              ],
            product =
              ( PRODUCT_FROM {amount = 10.8, inputEntity = Nothing, outputEntity = Nothing},
                M (Molecule {id = Nothing, smiles = pack "mol3_smile", iupacName = pack "mol3_iupac"})
              ),
            catalysts =
              [ ( ACCELERATE {temperature = 5.0, pressure = 6.0, catalyst = Nothing, reaction = Nothing},
                  C Catalyst {id = Nothing, smiles = pack "cat1_smile", name = Just . pack $ "cat1"}
                )
              ]
          }
  startTime <- getCurrentTime
  -- setupDB appEnv
  let maybeIds = do
        id1 <- fromString "0636aeb3-56af-4097-8749-ab22e8675248"
        id2 <- fromString "d633261f-53f4-40cc-b3b1-b128763edad3"
        Just (id1, id2)
  case maybeIds of
    Just (id1, id2) -> do
      pathNodeList <- getPath appEnv id1 id2
      logMsg logger (show pathNodeList)
  finishTime <- getCurrentTime
  logMsg logger ("Work time: " ++ show (diffUTCTime finishTime startTime))

loadSettings :: IO Settings
loadSettings = do
  _ <- loadFile defaultConfig
  neo4j_user <- T.pack <$> getEnv "NEO4J_USER"
  neo4j_pass <- T.pack <$> getEnv "NEO4J_PASS"
  neo4j_host <- getEnv "NEO4J_HOST"
  neo4j_port <- read <$> getEnv "NEO4J_PORT"
  return Settings {..}

wrapLogMsg :: String -> IO LogStr
wrapLogMsg msg = do
  currentTime <- getCurrentTime
  return . toLogStr $ show currentTime <> " " <> msg <> "\n"
