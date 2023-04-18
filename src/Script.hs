{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Script where

import API.Models
  ( ACCELERATE (ACCELERATE),
    Catalyst (Catalyst),
    CatalystOrUUID (CU),
    Molecule (Molecule, id),
    MoleculeOrUUID (MU),
    PRODUCT_FROM (PRODUCT_FROM),
    Reaction (Reaction),
    ReactionInput (ReactionInput, catalysts, product, reaction, reagents),
    getCatalystId,
  )
import Control.Monad (forM, forM_, (>=>))
import Data.Fixed (mod')
import Data.Maybe (fromMaybe)
import Data.UUID (nil, toString)
import Data.UUID.V4 (nextRandom)
import External.Interfaces (AppEnvironment (AppEnvironment, db), Neo4jConn (createNode, createReaction))
import System.Random (randomIO)
import Prelude hiding (id)

setupDB :: AppEnvironment -> IO ()
setupDB AppEnvironment {..} = do
  reactions <- forM [1 .. 20] (const genRandom) :: IO [Reaction]
  molecules <- forM [1 .. 20] (const genRandom) :: IO [Molecule]
  catalysts <- forM [1 .. 20] (const genRandom) :: IO [Catalyst]
  forM_ molecules (createNode db)
  forM_ catalysts (createNode db)
  let genReactionInput reaction = do
        reactProduct <- pickRandElement molecules []
        reactProductRelation <- genRandom
        reactReagents <- pickRandElements molecules [reactProduct]
        reactReagentRelations <- forM [1 .. length reactReagents] (const genRandom)
        reactCatalysts <- pickRandElements catalysts []
        reactCatalystRelations <- forM [1 .. length reactCatalysts] (const genRandom)
        let reactProductId = fromMaybe nil (id reactProduct)
            reactReagentIds = map (MU . fromMaybe nil . id) reactReagents
            reactCataystIds = map (CU . fromMaybe nil . getCatalystId) reactCatalysts
        return $
          ReactionInput
            { product = (reactProductRelation, MU reactProductId),
              reaction = reaction,
              reagents = zip reactReagentRelations reactReagentIds,
              catalysts = zip reactCatalystRelations reactCataystIds
            }
  forM_ reactions (genReactionInput >=> createReaction db)

pickRandElements :: Eq a => [a] -> [a] -> IO [a]
pickRandElements elements filterList = do
  let elementList = filter (`notElem` filterList) elements
  resultListLen <- (+ 1) . (`mod` 3) <$> newRandInt
  let foldAction 0 result = return result
      foldAction counter result = do
        newElem <- pickRandElement elementList result
        foldAction (counter - 1) (newElem : result)
  foldAction resultListLen []

pickRandElement :: (Foldable t, Eq b) => [b] -> t b -> IO b
pickRandElement elements filterList = do
  let elementList = filter (`notElem` filterList) elements
  i <- (`mod` length elementList) <$> newRandInt
  return $ elementList !! i

newRandInt :: IO Int
newRandInt = randomIO :: IO Int

newRandFloat :: IO Float
newRandFloat = randomIO :: IO Float

class GenRandomElement a where
  genRandom :: IO a

instance GenRandomElement Reaction where
  genRandom = do
    rId <- nextRandom
    let name = "name " ++ toString rId
    return $ Reaction (Just rId) name

instance GenRandomElement Molecule where
  genRandom = do
    rId <- nextRandom
    let smiles = "smiles " ++ toString rId
        iupac = "iupac " ++ toString rId
    return $ Molecule (Just rId) smiles iupac

instance GenRandomElement Catalyst where
  genRandom = do
    rId <- nextRandom
    let smiles = "smiles " ++ toString rId
        name = Just $ "iupac " ++ toString rId
    return $ Catalyst (Just rId) smiles name

instance GenRandomElement PRODUCT_FROM where
  genRandom = do
    amount <- (`mod'` 100) <$> newRandFloat
    return $ PRODUCT_FROM amount Nothing Nothing

instance GenRandomElement ACCELERATE where
  genRandom = do
    temperature <- (`mod'` 100) <$> newRandFloat
    pressure <- (`mod'` 100) <$> newRandFloat
    return $ ACCELERATE temperature pressure Nothing Nothing
