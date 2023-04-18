{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module External.Neo4j where

import API.Models
  ( ACCELERATE (ACCELERATE, catalyst, pressure, reaction, temperature),
    CatalystOrUUID (C, CU),
    MoleculeOrUUID (M, MU),
    PRODUCT_FROM (PRODUCT_FROM, amount, inputEntity, outputEntity),
    Reaction (Reaction),
    ReactionInput
      ( ReactionInput,
        catalysts,
        product,
        reaction,
        reagents
      ),
  )
import Control.Monad (forM, forM_)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Default ()
import Data.Map (insert)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID, fromString, nil, toString)
import Data.UUID.V4 (nextRandom)
import Database.Bolt (BoltActionT, Node (nodeProps), Pipe, Value (T), at, props, queryP, queryP_, run, (=:))
import External.Interfaces (Neo4jConn (..), ReactionElement (getCheckExistsQueryText, getCreateQueryProps, getCreateQueryText, getElementId))

newtype Neo4jDB = Neo4jDB {boltPipe :: Pipe}

instance Neo4jConn Neo4jDB where
  createReaction db (ReactionInput {..}) = do
    reactionUuid <- createNode db reaction
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
            M molecule -> createNode db molecule
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
            C catalyst -> createNode db catalyst
            CU cId -> return (Just cId)
          case maybeCatalystId of
            Just catalystId -> do
              let createRelationQuery = do
                    queryP_
                      "MATCH (cat:Catalyst),(react:Reaction) WHERE cat.id = {catId} AND react.id = {reactId} \
                      \CREATE (cat)-[r:ACCELERATE {temperature: {temperature}, pressure: {pressure}}]->(react)"
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
        return rId
      Nothing -> return nil

  createNode db element = do
    predefinedId <- case getElementId element of
      Just elId ->
        runMaybeT $ checkIdExists db element elId
      Nothing -> return Nothing
    newUUID <- nextRandom
    let queryProps = case predefinedId of
          Just elId -> insert (T.pack "id") (T . T.pack . toString $ elId) (getCreateQueryProps element)
          Nothing -> insert (T.pack "id") (T . T.pack . toString $ newUUID) (getCreateQueryProps element)
        createNodeQuery = do
          records <- queryP (getCreateQueryText element) queryProps
          forM records $ \record -> record `at` "r.id" :: BoltActionT IO Text
    result <- run (boltPipe db) createNodeQuery
    unpackUUID result
    where
      checkIdExists d e eId = MaybeT $ do
        res <- checkNodeExistsById d e eId
        if res
          then return Nothing
          else return $ Just eId

  checkNodeExistsById db element elementId = do
    let checkQuery = do
          records <-
            queryP (getCheckExistsQueryText element) (props ["elId" =: toString elementId])
          forM records $ \record -> record `at` "node_exists" :: BoltActionT IO Bool
    checkResult <- run (boltPipe db) checkQuery
    if null checkResult
      then return False
      else return . head $ checkResult

  getReactionNodeById db reactionId = do
    let getReactionQuery = do
          records <- queryP "MATCH (r:Reaction) WHERE r.id = {rId} RETURN r" (props ["rId" =: toString reactionId])
          nodes <- forM records $ \record -> record `at` "r"
          forM nodes genReaction
        genReaction node = do
          rName <- T.unpack <$> nodeProps node `at` "name"
          rId <- fromString . T.unpack <$> nodeProps node `at` "id"
          return $ Reaction rId rName

    reactList <- run (boltPipe db) getReactionQuery
    if null reactList
      then return Nothing
      else return . Just $ head reactList

unpackUUID :: [Text] -> IO (Maybe UUID)
unpackUUID result = do
  if null result
    then return Nothing
    else return . fromString . T.unpack $ head result
