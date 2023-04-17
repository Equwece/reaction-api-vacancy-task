{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module External.Interfaces where

import API.Models (Catalyst (Catalyst, id, name, smiles), Molecule (Molecule, id, iupacName, smiles), Reaction (Reaction, id, name), ReactionInput)
import Data.Map
import Data.Text (Text)
import Data.UUID (UUID)
import Database.Bolt (Value, props, (=:))

data AppEnvironment where
  AppEnvironment ::
    (Neo4jConn b) =>
    {logger :: Logger, db :: b} ->
    AppEnvironment

newtype Logger = Logger {logMsg :: String -> IO ()}

class Neo4jConn a where
  createReaction :: a -> ReactionInput -> IO UUID
  createNode :: (ReactionElement element) => a -> element -> IO (Maybe UUID)
  getReactionNodeById :: a -> UUID -> IO (Maybe Reaction)
  checkNodeExistsById :: (ReactionElement element) => a -> element -> UUID -> IO Bool

class ReactionElement b where
  getCreateQueryProps :: b -> Map Text Value
  getCreateQueryText :: b -> Text
  getElementId :: b -> Maybe UUID
  getCheckExistsQueryText :: b -> Text

instance ReactionElement Catalyst where
  getCreateQueryProps Catalyst {..} = props ["name" =: name, "smiles" =: smiles]
  getCreateQueryText Catalyst {} = "CREATE (r:Catalyst {name: {name}, smiles: {smiles}, id: {id}}) RETURN r.id"
  getElementId Catalyst {..} = id
  getCheckExistsQueryText Catalyst {} = "MATCH (e:Catalyst {id: {elId}}) WITH COUNT(e) > 0 as node_exists RETURN node_exists"

instance ReactionElement Reaction where
  getCreateQueryProps Reaction {..} = props ["name" =: name]
  getCreateQueryText Reaction {} = "CREATE (r:Reaction {name: {name}, id: {id}}) RETURN r.id"
  getElementId Reaction {..} = id
  getCheckExistsQueryText Reaction {} = "MATCH (e:Catalyst {id: {elId}}) WITH COUNT(e) > 0 as node_exists RETURN node_exists"

instance ReactionElement Molecule where
  getCreateQueryProps Molecule {..} = props ["iupacName" =: iupacName, "smiles" =: smiles]
  getCreateQueryText Molecule {} = "CREATE (r:Molecule {iupacName: {iupacName}, smiles: {smiles}, id: {id}}) RETURN r.id"
  getElementId Molecule {..} = id
  getCheckExistsQueryText Molecule {} = "MATCH (e:Catalyst {id: {elId}}) WITH COUNT(e) > 0 as node_exists RETURN node_exists"
