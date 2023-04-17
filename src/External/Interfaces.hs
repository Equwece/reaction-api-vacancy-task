{-# LANGUAGE GADTs #-}

module External.Interfaces where

data AppEnvironment where
  AppEnvironment ::
    (Neo4jConn b) =>
    {logger :: Logger, db :: b} ->
    AppEnvironment

newtype Logger = Logger {logMsg :: String -> IO ()}

class Neo4jConn a
