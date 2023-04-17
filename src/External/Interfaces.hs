{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module External.Interfaces where

import API.Models (Catalyst (Catalyst, name, smiles), Molecule (Molecule, iupacName, smiles), Reaction (Reaction, name), ReactionInput)
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
  createReaction :: a -> ReactionInput -> IO ()
  createNode :: (ReactionElement label) => a -> label -> IO (Maybe UUID)
  getReactionNodeById :: a-> UUID -> IO (Maybe Reaction)

class ReactionElement b where
  getCreateQueryProps :: b -> Map Text Value
  getCreateQueryText :: b -> Text

instance ReactionElement Catalyst where
  getCreateQueryProps Catalyst {..} = props ["name" =: name, "smiles" =: smiles]
  getCreateQueryText Catalyst {} = "CREATE (r:Catalyst {name: {name}, smiles: {smiles}, id: randomUUID()}) RETURN r.id"

instance ReactionElement Reaction where
  getCreateQueryProps Reaction {..} = props ["name" =: name]
  getCreateQueryText Reaction {} = "CREATE (r:Reaction {name: {name}, id: randomUUID()}) RETURN r.id"

instance ReactionElement Molecule where
  getCreateQueryProps Molecule {..} = props ["iupacName" =: iupacName, "smiles" =: smiles]
  getCreateQueryText Molecule {} = "CREATE (r:Molecule {iupacName: {iupacName}, smiles: {smiles}, id: randomUUID()}) RETURN r.id"
