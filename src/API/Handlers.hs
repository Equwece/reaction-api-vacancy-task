{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module API.Handlers (resultServer, reactionApp) where

import API.APISpec (ReactionAPI, RestAPI, proxyAPI)
import API.Models (PathNode, Reaction, ReactionInput)
import Control.Lens ((&), (.~), (?~))
import Control.Monad.Cont (MonadIO (liftIO))
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (StateT (runStateT))
import Data.OpenApi (HasInfo (info), HasLicense (license), HasServers (servers), HasTitle (title), HasVersion (version), OpenApi)
import Data.UUID (UUID)
import External.Interfaces
import Servant
  ( Application,
    Handler,
    Proxy (..),
    Server,
    err404,
    serve,
    type (:<|>) ((:<|>)),
  )
import Servant.OpenApi (HasOpenApi (toOpenApi))
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

reactionApp :: (Neo4jConn a) => AppEnvironment a -> Application
reactionApp appEnv = serve proxyAPI (resultServer appEnv)

resultServer :: (Neo4jConn a) => AppEnvironment a -> Server ReactionAPI
resultServer appEnv = (searchServer appEnv :<|> reactionServer appEnv) :<|> swaggerServer

swaggerServer :: Server (SwaggerSchemaUI api b)
swaggerServer = swaggerSchemaUIServer openApiSpec

openApiSpec :: OpenApi
openApiSpec =
  toOpenApi (Proxy :: Proxy RestAPI)
    & info . title .~ "Reaction API"
    & info . version .~ "1.0"
    & info . license ?~ "LGPL"
    & servers .~ ["http://localhost:8090/api/v1"]

reactionServer :: (Neo4jConn a) => AppEnvironment a -> (ReactionInput -> Handler UUID) :<|> (UUID -> Handler Reaction)
reactionServer appEnv@(AppEnvironment {..}) = createReactionServer :<|> reactionEntityServer appEnv
  where
    createReactionServer :: ReactionInput -> Handler UUID
    createReactionServer react = do
      (reactId, _) <- liftIO $ runStateT (createReaction appEnv react) db
      liftIO $ logMsg logger ("Create Reaction " <> show reactId)
      return reactId

reactionEntityServer :: (Neo4jConn a) => AppEnvironment a -> UUID -> Handler Reaction
reactionEntityServer appEnv@(AppEnvironment {..}) resId = getReaction resId
  where
    getReaction :: UUID -> Handler Reaction
    getReaction rId = do
      (react, _) <- liftIO $ runStateT (getReactionNodeById appEnv rId) db
      liftIO $ logMsg logger ("Get Reaction " <> show rId)
      case react of
        Just reaction -> return reaction
        Nothing -> throwError err404

searchServer :: Neo4jConn a => AppEnvironment a -> (UUID, UUID) -> Handler [[PathNode]]
searchServer appEnv@AppEnvironment {..} (id1, id2) = do
  (searchResult, _) <- liftIO $ runStateT (getPath appEnv id1 id2) db
  liftIO $ logMsg logger ("Search patch: " <> show id1 <> " " <> show id2)
  if null searchResult
    then throwError err404
    else return searchResult
