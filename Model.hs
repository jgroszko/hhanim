{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Model ( IndexedFaceSet
             , coordIndex
             , coordIndexPtr
             , coordinate
             , pointPtr
             , loadIndexedFaceSet
             , listToTriangles
             ) where

import Char
import Control.Arrow

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

import Foreign (Ptr, newArray)

import Data.List.Split

import Graphics.UI.GLUT

data IndexedFaceSet = IndexedFaceSet
                      { solid :: Bool
                      , creaseAngle :: Float
                      , texCoordIndex :: [Int]
                      , coordIndex :: [GLuint]
                      , coordIndexPtr :: Ptr GLuint
                      , coordinate :: Maybe Coordinate
                      }
                      deriving (Show)

data Coordinate = Coordinate
                  { def :: String
                  , point :: [Float]
                  , pointPtr :: Ptr Float
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
                      returnA -< IndexedFaceSet { solid = solid
                                                , creaseAngle = creaseAngle
                                                , texCoordIndex = texCoordIndex
                                                , coordIndex = coordIndex
                                                , coordIndexPtr = coordIndicesPtr
                                                , coordinate = Just coordinate }

getCoordinate = atTag "Coordinate"
                >>>
                proc x -> do                 
                  points <- stringToList <<< getAttrValue "point" -< x
                  pointsPtr <- (arrIO newArray) -< points
                  def <- getAttrValue "DEF" -< x
                  returnA -< Coordinate { point = points, pointPtr = pointsPtr, def = def }

loadIndexedFaceSet :: String -> IO [IndexedFaceSet]
loadIndexedFaceSet file = runX (
  readDocument [ withValidate no 
               , withCurl [] ]
    file
  >>>
  getIndexedFaceSet
  )
