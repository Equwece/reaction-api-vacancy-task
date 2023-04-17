{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module External.Neo4j where

import API.Models (ACCELERATE (ACCELERATE, catalyst, pressure, temperature), Catalyst (Catalyst, name, smiles), CatalystOrUUID (C, CU), Molecule (Molecule, iupacName, smiles), MoleculeOrUUID (M, MU), PRODUCT_FROM (PRODUCT_FROM, amount), REAGENT_IN, Reaction (Reaction, name), ReactionInput (ReactionInput, catalysts, product, reaction, reagents))
import Control.Monad (forM, forM_)
import Control.Monad.Except ()
import Data.Default ()
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, fromString, toString)
import Database.Bolt (BoltActionT, Pipe, at, props, query, queryP, queryP_, query_, run, (=:))
import External.Interfaces (Neo4jConn (createCatalystNode, createMoleculeNode, createReaction, createReactionNode))

newtype Neo4jDB = Neo4jDB {boltPipe :: Pipe}

instance Neo4jConn Neo4jDB where
  createReaction db (ReactionInput {..}) = do
    reactionUuid <- createReactionNode db reaction
    let processReagent = processComponentMolecule queryStr
          where
            queryStr =
              "MATCH (mol:Molecule),(react:Reaction) WHERE mol.id = {molId} AND react.id = {reactId} \
              \CREATE (mol)-[r:REAGENT_IN {amount: {amount}}]->(react)"
        processProduct = processComponentMolecule queryStr
          where
            queryStr =
              "MATCH (mol:Molecule),(react:Reaction) WHERE mol.id = {molId} AND react.id = {reactId} \
              \CREATE (react)-[r:PRODUCT_FROM {amount: {amount}}]->(mol)"
        processComponentMolecule queryStr (PRODUCT_FROM {..}, moleculeOrId) reactId = do
          maybeMoleculeId <- case moleculeOrId of
            M molecule -> createMoleculeNode db molecule
            MU mId -> return (Just mId)
          case maybeMoleculeId of
            Just moleculeId -> do
              let createRelationQuery = do
                    queryP_
                      queryStr
                      (props ["molId" =: toString moleculeId, "reactId" =: toString reactId, "amount" =: amount])
              run (boltPipe db) createRelationQuery
            Nothing -> return ()
        processCatalyst (ACCELERATE {..}, catalystOrId) reactId = do
          maybeCatalystId <- case catalystOrId of
            C catalyst -> createCatalystNode db catalyst
            CU cId -> return (Just cId)
          case maybeCatalystId of
            Just catalystId -> do
              let createRelationQuery = do
                    queryP_
                      "MATCH (cat:Catalyst),(react:Reaction) WHERE cat.id = {catId} AND react.id = {reactId} \
                      \CREATE (react)-[r:ACCELERATE {temperature: {temperature}, pressure: {pressure}}]->(cat)"
                      ( props
                          [ "catId" =: toString catalystId,
                            "reactId" =: toString reactId,
                            "temperature" =: temperature,
                            "pressure" =: pressure
                          ]
                      )
              run (boltPipe db) createRelationQuery
            Nothing -> return ()
    case reactionUuid of
      Just rId -> do
        forM_ reagents (`processReagent` rId)
        processProduct product rId
        forM_ catalysts (`processCatalyst` rId)
      Nothing -> return ()

  createReactionNode db Reaction {..} = do
    let createNodeQuery = do
          records <- queryP "CREATE (r:Reaction {name: {name}, id: randomUUID()}) RETURN r.id" (props ["name" =: name])
          forM records $ \record -> record `at` "r.id" :: BoltActionT IO Text

    result <- run (boltPipe db) createNodeQuery
    unpackUUID result

  createMoleculeNode db Molecule {..} = do
    let createNodeQuery = do
          records <-
            queryP
              "CREATE (r:Molecule {iupacName: {iupacName}, smiles: {smiles}, id: randomUUID()}) RETURN r.id"
              (props ["iupacName" =: iupacName, "smiles" =: smiles])
          forM records $ \record -> record `at` "r.id" :: BoltActionT IO Text

    result <- run (boltPipe db) createNodeQuery
    unpackUUID result

  createCatalystNode db Catalyst {..} = do
    let createNodeQuery = do
          records <-
            queryP
              "CREATE (r:Catalyst {name: {name}, smiles: {smiles}, id: randomUUID()}) RETURN r.id"
              (props ["name" =: name, "smiles" =: smiles])
          forM records $ \record -> record `at` "r.id" :: BoltActionT IO Text
    result <- run (boltPipe db) createNodeQuery
    unpackUUID result

unpackUUID :: [Text] -> IO (Maybe UUID)
unpackUUID result = do
  if null result
    then return Nothing
    else return . fromString . T.unpack $ head result
