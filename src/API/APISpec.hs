{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.APISpec where

import API.Models (PathNode (PathNode), Reaction, ReactionInput)
import Data.UUID (UUID)
import Servant
  ( Capture,
    Get,
    JSON,
    Post,
    Proxy (..),
    ReqBody,
    type (:<|>),
    type (:>),
  )
import Servant.Swagger.UI (SwaggerSchemaUI)

type ReactionAPI = "api" :> "v1" :> (RestAPI :<|> SwaggerAPI)

type SwaggerAPI =
  SwaggerSchemaUI "docs" "openapi.json"

type RestAPI = "search" :> ReqBody '[JSON] (UUID, UUID) :> Post '[JSON] [[PathNode]] :<|> ReactionsAPI

type ReactionsAPI =
  "reactions"
    :> ( ReqBody '[JSON] ReactionInput :> Post '[JSON] UUID
           :<|> Capture "userId" UUID :> Get '[JSON] Reaction
       )

proxyAPI :: Proxy ReactionAPI
proxyAPI = Proxy
