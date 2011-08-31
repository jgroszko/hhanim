{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module Model ( X3DIndexedFaceSet
             , ifsVertices
             , ifsIndices
             , ifsIndicesCount
             , ifsNormals
             , ifsTexCoords
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
    { ifsVertices :: Ptr Float
    , ifsIndices :: Ptr GLuint
    , ifsIndicesCount :: Int
    , ifsTexCoords :: Maybe (Ptr Float)
    , ifsNormals :: Maybe (Ptr Float)
    }
                      deriving (Show)

data X3DCoordinate = X3DCoordinate
    { cPoint :: [Float]
    }
                  deriving (Show)

data X3DNormal = X3DNormal
    { nVector :: [Float]
    }
              deriving (Show)

data X3DTextureCoordinate = X3DTextureCoordinate
    { tcPoint :: [Float]
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

mixedListToTriangles :: [[GLuint]] -> [GLuint]
mixedListToTriangles ([]) = []
mixedListToTriangles ((a:b:c:[]):xs) = [a,b,c] ++ (mixedListToTriangles xs)
mixedListToTriangles ((a:b:c:d:[]):xs) = [a,b,c,a,d,c] ++ (mixedListToTriangles xs)

listToTriangles :: (Arrow a) => a [GLuint] [GLuint]
listToTriangles = proc x -> do
                     returnA -< mixedListToTriangles (endBy [(-1)] x)

atTag tag = deep (isElem >>> hasName tag)

maybeArray = arrIO (\x -> case x of
                            Nothing -> do
                              return Nothing
                            Just y -> do
                              arr <- newArray y
                              return (Just arr))

maybeProperty f = arrIO (\x -> case x of
                                 Nothing -> do
                                   return Nothing
                                 Just y -> do
                                   return (Just (f y)))

processTexCoords = arr (\ (vertices, normals, indices, texCoords, texCoordIndices) ->
                            ((foldl (\vs ui ->
                                         let i = fromIntegral ui in
                                         vs ++ [vertices !! (i*3)
                                               , vertices !! ((i*3)+1)
                                               , vertices !! ((i*3)+2)]
                                    )
                              []
                              indices),
                             case normals of
                               Nothing -> Nothing
                               Just normals ->
                                   Just (foldl (\ns ui ->
                                                    let i = fromIntegral ui in
                                                    ns ++ [ normals !! (i*3)
                                                          , normals !! ((i*3)+1)
                                                          , normals !! ((i*3)+2)]
                                               )
                                         []
                                         indices),
                             [0..(fromIntegral (length indices))::GLuint],
                             case texCoords of
                               Nothing -> Nothing
                               Just texCoords ->
                                   Just (foldl (\tcs ui -> 
                                                    let i = (fromIntegral ui)::Int in
                                                    tcs ++ [ texCoords !! (i*2)
                                                           , texCoords !! ((i*2)+1) ])
                                         []
                                         texCoordIndices)
                            ))

getIndexedFaceSet = atTag "IndexedFaceSet"
                    >>>
                    proc x -> do
                      coordinate <- getCoordinate -< x
                      vertices <- arr cPoint -< coordinate

                      indices <- listToTriangles <<< stringToList <<< getAttrValue "coordIndex" -< x
                      facesCount <- (arr length) -< indices

                      normal <- getNormal -< x
                      normals <- maybeProperty nVector -< normal

                      texCoord <- getTexCoord -< x
                      texCoords <- maybeProperty tcPoint -< texCoord
                      texCoordIndices <- listToTriangles <<< stringToList <<< getAttrValue "texCoordIndex" -< x

                      (vertices, normals, indices, texCoords) <- processTexCoords -<
                                                        (vertices, normals, indices, texCoords, texCoordIndices)

                      finalVertices <- (arrIO newArray) -< vertices
                      finalIndices <- (arrIO newArray) -< indices
                      finalTexCoords <- maybeArray -< texCoords
                      finalNormals <- maybeArray -< normals

                      returnA -< X3DIndexedFaceSet { ifsVertices = finalVertices
                                                   , ifsIndices = finalIndices
                                                   , ifsIndicesCount = facesCount
                                                   , ifsTexCoords = finalTexCoords
                                                   , ifsNormals = finalNormals
                                                   }

getCoordinate = atTag "Coordinate"
                >>>
                proc x -> do                 
                  points <- stringToList <<< getAttrValue "point" -< x
                  returnA -< X3DCoordinate { cPoint = points }

getTexCoord = (atTag "TextureCoordinate"
               >>>
               proc x -> do
                 points <- stringToList <<< getAttrValue "point" -< x
                 returnA -< Just X3DTextureCoordinate { tcPoint = points }
              ) `orElse` (constA Nothing)

getNormal = (atTag "Normal"
            >>>
            proc x -> do
              vectors <- stringToList <<< getAttrValue "vector" -< x
              returnA -< Just X3DNormal { nVector = vectors }
            ) `orElse` (constA Nothing)

loadIndexedFaceSet :: String -> IO [X3DIndexedFaceSet]
loadIndexedFaceSet file = runX (
  readDocument [ withValidate no 
               , withCurl [] ]
    file
  >>>
  getIndexedFaceSet
  )
