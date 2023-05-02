{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import API.APISpec (RestAPI, proxyAPI, restAPI)
import API.Handlers (reactionApp, resultServer)
import API.Models
  ( ACCELERATE (ACCELERATE, catalyst, pressure, reaction, temperature),
    Catalyst (Catalyst, id, name, smiles),
    CatalystInput (CatalystInput, accelerate, catalyst),
    CatalystOrUUID (C),
    Molecule (Molecule, id, iupacName, smiles),
    MoleculeOrUUID (M),
    PRODUCT_FROM (PRODUCT_FROM, amount, inputEntity, outputEntity),
    ProductInput (ProductInput, molecule, relation),
    Reaction (Reaction, id, name),
    ReactionInput
      ( ReactionInput,
        catalysts,
        product,
        reaction,
        reagents
      ),
    ReagentInput (..),
  )
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (evaluate)
import Control.Monad (when, (>=>))
import Data.Default (Default (def))
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Data.Text as T
import Data.Time (getCurrentTime, getCurrentTimeZone)
import Data.UUID (fromString, nil)
import Database.Bolt (BoltCfg (host, password, port, user), connect)
import External.Interfaces
  ( AppEnvironment (..),
    Logger (Logger, logMsg),
    Neo4jConn,
  )
import External.Neo4j (Neo4jDB (Neo4jDB))
import External.Settings (Settings (..))
import Network.HTTP.Client (Proxy (Proxy), defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp (run, testWithApplication)
import Servant (Application, NoContent (NoContent), err404, serve, type (:<|>) ((:<|>)))
import Servant.Client
  ( BaseUrl (baseUrlPort),
    ClientError (FailureResponse),
    client,
    mkClientEnv,
    parseBaseUrl,
    runClientM,
  )
import Servant.Client.Core.Request (RequestF (requestPath))
import SetupDB (setupDB)
import System.Environment (getEnv)
import System.Log.FastLogger (LogStr, LogType' (LogStdout), ToLogStr (toLogStr), defaultBufSize, withFastLogger)
import Test.HUnit.Lang (assertFailure)
import Test.Hspec
  ( Spec,
    around,
    describe,
    hspec,
    it,
    runIO,
    shouldBe,
  )
import Test.QuickCheck (Testable (property))

main :: IO ()
main = do
  withFastLogger (LogStdout defaultBufSize) $ \fastLogger -> do
    settings <- loadTestSettings
    let neo4jConnCfg =
          def
            { user = neo4jUser settings,
              password = neo4jPass settings,
              host = neo4jHost settings,
              port = neo4jPort settings
            }

    db <- Neo4jDB <$> connect neo4jConnCfg
    let logger = Logger {logMsg = fastLogger . toLogStr}
        appEnv = AppEnvironment {..}
    hspec (spec appEnv)

spec = apiSpec

loadTestSettings :: IO Settings
loadTestSettings = do
  _ <- loadFile defaultConfig
  neo4jUser <- T.pack <$> getEnv "TEST_NEO4J_USER"
  neo4jPass <- T.pack <$> getEnv "TEST_NEO4J_PASS"
  neo4jHost <- getEnv "TEST_NEO4J_HOST"
  neo4jPort <- read <$> getEnv "TEST_NEO4J_PORT"
  appPort <- read <$> getEnv "APP_PORT"
  setupDBFlag <- read <$> getEnv "SETUP_DB"
  return Settings {..}

withUserApp appEnv = testWithApplication (pure (reactionApp appEnv))

apiSpec :: (Neo4jConn b) => AppEnvironment b -> Spec
apiSpec appEnv@AppEnvironment {..} =
  around (withUserApp appEnv) $ do
    let ((searchClient :<|> (postReactionClient :<|> getReactionClient)) :<|> swaggerClient) = client proxyAPI
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})
        getUsersRequest = requestPath
        testReactId = fromMaybe nil $ fromString "b86943c9-264d-4181-bda7-4830fd650527"

    describe "POST /api/v1/reactions" $ do
      it "should create new reaction" $ \port -> do
        result <- runClientM (postReactionClient testReactionInput) (clientEnv port)
        result `shouldBe` Right testReactId

    describe "GET /api/v1/reactions/:id" $ do
      it "should get reaction by id" $ \port -> do
        result <- runClientM (getReactionClient testReactId) (clientEnv port)
        result `shouldBe` Right testReaction

    describe "POST /api/v1/search" $ do
      it "should find path between given molecules" $ \port -> do
        let testId1 = fromMaybe nil $ fromString "84c10bb1-fb60-4628-ac02-6043a04638c9"
            testId2 = fromMaybe nil $ fromString "4ea244aa-19cf-4d9a-819c-b52fb7a1a116"
        result <- runClientM (searchClient (testId1, testId2)) (clientEnv port)
        case result of
          Left _ -> assertFailure "Search Failure"
          Right _ -> return ()

testReaction = Reaction {name = pack "react1", id = fromString "b86943c9-264d-4181-bda7-4830fd650527"}

testReactionInput =
  ReactionInput
    { reaction = testReaction,
      reagents =
        [ ( ProductInput
              { relation = PRODUCT_FROM {amount = 50.3, inputEntity = Nothing, outputEntity = Nothing},
                molecule =
                  M
                    ( Molecule
                        { id = fromString "84c10bb1-fb60-4628-ac02-6043a04638c9",
                          smiles = pack "mol1_smile",
                          iupacName = pack "mol1_iupac"
                        }
                    )
              }
          ),
          ( ProductInput
              { relation = PRODUCT_FROM {amount = 40.8, inputEntity = Nothing, outputEntity = Nothing},
                molecule = M (Molecule {id = Nothing, smiles = pack "mol2_smile", iupacName = pack "mol2_iupac"})
              }
          )
        ],
      product =
        ( ProductInput
            { relation = PRODUCT_FROM {amount = 10.8, inputEntity = Nothing, outputEntity = Nothing},
              molecule =
                M
                  ( Molecule
                      { id = fromString "4ea244aa-19cf-4d9a-819c-b52fb7a1a116",
                        smiles = pack "mol3_smile",
                        iupacName = pack "mol3_iupac"
                      }
                  )
            }
        ),
      catalysts =
        [ ( CatalystInput
              { accelerate = ACCELERATE {temperature = 5.0, pressure = 6.0, catalyst = Nothing, reaction = Nothing},
                catalyst = C Catalyst {id = Nothing, smiles = pack "cat1_smile", name = Just . pack $ "cat1"}
              }
          )
        ]
    }
