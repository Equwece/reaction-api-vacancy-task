{-# LANGUAGE GADTs #-}

module External.Interfaces where

import API.Models (Molecule, Reaction, ReactionInput, Catalyst)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.Bolt (BoltActionT)

data AppEnvironment where
  AppEnvironment ::
    (Neo4jConn b) =>
    {logger :: Logger, db :: b} ->
    AppEnvironment

newtype Logger = Logger {logMsg :: String -> IO ()}

class Neo4jConn a where
  createReaction :: a -> ReactionInput -> IO ()
  createReactionNode :: a -> Reaction -> IO (Maybe UUID)
  createMoleculeNode :: a -> Molecule -> IO (Maybe UUID)
  createCatalystNode :: a -> Catalyst -> IO (Maybe UUID)
