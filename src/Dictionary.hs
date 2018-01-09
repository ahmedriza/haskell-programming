{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Dictionary where

import Control.Applicative
import Control.Monad
import qualified Data.Map.Strict as Map

dataSetDetail :: Maybe DataSetDetail
dataSetDetail = Just (
  DataSetDetail "ds1"
    [
      Chunk "ch1" (ChunkDetail "chunk 1")
    , Chunk "ch2" (ChunkDetail "chunk 2")
    ])

data DataSetDetail = DataSetDetail {
  dsId :: String
  , chunks :: [Chunk]
  } deriving (Show)

data Chunk = Chunk {
  chunkId :: String
  , chunkDetail :: ChunkDetail
  } deriving (Show)

data ChunkDetail = ChunkDetail {
  chunkName :: String
  } deriving (Show)

parseChunk :: Chunk -> Maybe Chunk
parseChunk chunk = result
  where cid = chunkId chunk
        result = case cid of
          "ch2" -> Nothing
          otherwise -> Just chunk

insertChunk :: Maybe Chunk -> Map.Map String ChunkDetail -> Map.Map String ChunkDetail
insertChunk chunkMaybe dict = case chunkMaybe of
  Just ch -> Map.insert (chunkId ch) (chunkDetail ch) dict
  otherwise -> dict

main :: IO ()
main = do
  let dsd :: Maybe DataSetDetail = dataSetDetail
      chunks' :: Maybe [Chunk] = liftA chunks dsd
  print chunks'

  -- (Chunk -> Maybe Chunk) -> Maybe [Chunk] 
  let chunks'' :: Maybe [Maybe Chunk] = (fmap parseChunk <$>) chunks'
  print chunks''
  
  let dict' :: Map.Map String ChunkDetail = Map.fromList []
  -- let tmp = foldr insertChunk dict' chunks'
  -- print tmp
  
  return ()
