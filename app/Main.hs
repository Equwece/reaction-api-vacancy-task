{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad ((>=>))
import Control.Monad.Except ()
import Data.Default (Default (def))
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Bolt (BoltCfg (host, password, port, user), connect)
import External.Interfaces (AppEnvironment (..), Logger (Logger, logMsg))
import External.Neo4j (Neo4jDB (Neo4jDB))
import External.Settings (Settings (..))
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
