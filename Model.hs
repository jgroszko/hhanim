{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Model ( X3DIndexedFaceSet
             , ifsCoordIndex
             , ifsCoordIndexPtr
             , ifsCoordinate
             , cPointPtr
             , ifsNormal
             , nVectorPtr
             , loadIndexedFaceSet
             ) where

import Char
import Control.Arrow

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

import Foreign (Ptr, newArray)

import Data.List.Split

import Graphics.UI.GLUT

data X3DIndexedFaceSet = X3DIndexedFaceSet
    { ifsSolid :: Bool
    , ifsCreaseAngle :: Float
    , ifsTexCoordIndex :: [Int]
    , ifsCoordIndex :: [GLuint]
    , ifsCoordIndexPtr :: Ptr GLuint
    , ifsCoordinate :: Maybe X3DCoordinate
    , ifsNormal :: Maybe X3DNormal
    }
                      deriving (Show)

data X3DCoordinate = X3DCoordinate
    { cPoint :: [Float]
    , cPointPtr :: Ptr Float
    }
                  deriving (Show)

data X3DNormal = X3DNormal
    { nVector :: [Float]
    , nVectorPtr :: Ptr Float
    }
              deriving (Show)

stringToBool :: (Arrow a) => a String Bool
stringToBool = arr ( \ x -> (compare (map Char.toLower x) "true" == EQ) )

stringToFloat :: (Arrow a) => a String Float
stringToFloat = arr ( \ x -> read x::Float )

stringToList :: (Arrow a) => (Read b) => a String [b]
stringToList = arr ( \ x ->
                     map read $
                     words $
                     filter (/= ',') x )

listToTriples :: (Arrow a) => a [b] [[b]]
listToTriples = proc x -> do
                  returnA -< splitEvery 3 x

mixedListToTriangles :: (Num a) => [[a]] -> [a]
mixedListToTriangles ([]) = []
mixedListToTriangles ((a:b:c:[]):xs) = [a,b,c] ++ (mixedListToTriangles xs)
mixedListToTriangles ((a:b:c:d:[]):xs) = [a,b,c,a,d,c] ++ (mixedListToTriangles xs)

listToTriangles :: (Arrow a, Num b) => a [b] [b]
listToTriangles = proc x -> do
                     returnA -< mixedListToTriangles (endBy [(-1)] x)

atTag tag = deep (isElem >>> hasName tag)

getIndexedFaceSet = atTag "IndexedFaceSet"
                    >>>
                    proc x -> do
                      solid <- stringToBool <<< getAttrValue "solid" -< x
                      creaseAngle <- stringToFloat <<< getAttrValue "creaseAngle" -< x
                      texCoordIndex <- stringToList <<< getAttrValue "texCoordIndex" -< x
                      coordIndex <- listToTriangles <<< stringToList <<< getAttrValue "coordIndex" -< x
                      coordIndicesPtr <- (arrIO newArray) -< coordIndex
                      coordinate <- getCoordinate -< x
                      normal <- getNormal -< x
                      returnA -< X3DIndexedFaceSet { ifsSolid = solid
                                                   , ifsCreaseAngle = creaseAngle
                                                   , ifsTexCoordIndex = texCoordIndex
                                                   , ifsCoordIndex = coordIndex
                                                   , ifsCoordIndexPtr = coordIndicesPtr
                                                   , ifsCoordinate = Just coordinate
                                                   , ifsNormal = Just normal }

getCoordinate = atTag "Coordinate"
                >>>
                proc x -> do                 
                  points <- stringToList <<< getAttrValue "point" -< x
                  pointsPtr <- (arrIO newArray) -< points
                  returnA -< X3DCoordinate { cPoint = points
                                           , cPointPtr = pointsPtr }

getNormal = atTag "Normal"
            >>>
            proc x -> do
              vectors <- stringToList <<< getAttrValue "vector" -< x
              vectorsPtr <- (arrIO newArray) -< vectors
              returnA -< X3DNormal { nVector = vectors
                                   , nVectorPtr = vectorsPtr }

loadIndexedFaceSet :: String -> IO [X3DIndexedFaceSet]
loadIndexedFaceSet file = runX (
  readDocument [ withValidate no 
               , withCurl [] ]
    file
  >>>
  getIndexedFaceSet
  )
