module External.Neo4j where

import Database.Bolt (Pipe)
import External.Interfaces (Neo4jConn)

instance Neo4jConn Neo4jDB

newtype Neo4jDB = Neo4jDB {boltPipe :: Pipe}
