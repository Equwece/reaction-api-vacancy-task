{-# LANGUAGE RecordWildCards #-}

module Main where

import API.APISpec (proxyAPI)
import API.Handlers (resultServer)
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad (when, (>=>))
import Data.Default (Default (def))
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.Bolt (BoltCfg (host, password, port, user), connect)
import External.Interfaces
  ( AppEnvironment (..),
    Logger (Logger, logMsg),
    Neo4jConn,
  )
import External.Neo4j (Neo4jDB (Neo4jDB))
import External.Settings (Settings (..))
import Network.Wai.Handler.Warp (run)
import Servant (Application, serve)
import SetupDB (setupDB)
import System.Environment (getEnv)
import System.Log.FastLogger (LogStr, LogType' (LogStdout), ToLogStr (toLogStr), defaultBufSize, withFastLogger)

reactionApp :: (Neo4jConn b) => AppEnvironment b -> Application
reactionApp appEnv = serve proxyAPI (resultServer appEnv)

main :: IO ()
main = do
  withFastLogger (LogStdout defaultBufSize) $ \fastLogger -> do
    settings <- loadSettings
    let neo4jConnCfg =
          def
            { user = neo4jUser settings,
              password = neo4jPass settings,
              host = neo4jHost settings,
              port = neo4jPort settings
            }

    db <- Neo4jDB <$> connect neo4jConnCfg
    let logger = Logger {logMsg = wrapLogMsg >=> fastLogger}
        appEnv = AppEnvironment {..}
    when (setupDBFlag settings) $ do
      logMsg logger "Setup Neo4j DB with random data, it may take some time..."
      setupDB appEnv
      logMsg logger "DB setup is finished"
    logMsg logger "App has started..."
    run (appPort settings) (reactionApp appEnv)

loadSettings :: IO Settings
loadSettings = do
  _ <- loadFile defaultConfig
  neo4jUser <- T.pack <$> getEnv "NEO4J_USER"
  neo4jPass <- T.pack <$> getEnv "NEO4J_PASS"
  neo4jHost <- getEnv "NEO4J_HOST"
  neo4jPort <- read <$> getEnv "NEO4J_PORT"
  appPort <- read <$> getEnv "APP_PORT"
  setupDBFlag <- read <$> getEnv "SETUP_DB"
  return Settings {..}

wrapLogMsg :: String -> IO LogStr
wrapLogMsg msg = do
  currentTime <- getCurrentTime
  return . toLogStr $ "[" <> show currentTime <> "] " <> msg <> "\n"
