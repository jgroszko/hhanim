{-# LANGUAGE Arrows, ExistentialQuantification, NoMonomorphismRestriction #-}
module X3D.Geometry ( X3DGeometryNode (..)
                    , getGeometry
                    , drawGeometry
                    , X3DIndexedFaceSet (..)
                    , X3DCoordinate (..)
                    , X3DNormal (..)
                    , X3DTextureCoordinate (..)
                    ) where

import Control.Arrow
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Data.List.Split
import Foreign (Ptr, newArray)

import Graphics.UI.GLUT

import X3D.LoadUtil
import X3D.CalculateNormals

class X3DGeometryNode_ a where
    drawGeometry :: a -> IO ()

data X3DGeometryNode = forall a. (X3DGeometryNode_ a, Show a) => X3DGeometryNode a

instance X3DGeometryNode_ X3DGeometryNode where
    drawGeometry (X3DGeometryNode node) = drawGeometry node

instance Show X3DGeometryNode where
    show (X3DGeometryNode node) = show node

data X3DIndexedFaceSet = X3DIndexedFaceSet
    { ifsVertices :: Ptr Float
    , ifsIndices :: Ptr GLuint
    , ifsIndicesCount :: Int
    , ifsTexCoords :: Maybe (Ptr Float)
    , ifsNormals :: Maybe (Ptr Float)
    }
                      deriving (Show)

instance X3DGeometryNode_ X3DIndexedFaceSet where
    drawGeometry indexedFaceSet =
        let vertices = (ifsVertices indexedFaceSet)
            indices = (ifsIndices indexedFaceSet)
            indicesCount = fromIntegral (ifsIndicesCount indexedFaceSet)
        in do
          clientState VertexArray $= Enabled
          arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 vertices

          case (ifsNormals indexedFaceSet) of
            Nothing -> do clientState NormalArray $= Disabled
            Just normals -> do clientState NormalArray $= Enabled
                               arrayPointer NormalArray $= VertexArrayDescriptor 3 Float 0 normals

          case (ifsTexCoords indexedFaceSet) of
            Nothing -> do clientState TextureCoordArray $= Disabled
            Just texCoords -> do clientState TextureCoordArray $= Enabled
                                 arrayPointer TextureCoordArray $= VertexArrayDescriptor 2 Float 0 texCoords

          clientState IndexArray $= Enabled
          drawElements Triangles indicesCount UnsignedInt indices
  
          return ()

applyIndices points index dimensions = foldl (\xs ui ->
                                                  let i = fromIntegral ui in
                                                  xs ++ [ (points !! ((i*dimensions)+n)) | n <- [0..(dimensions-1)] ]
                                             )
                                       []
                                       index                              

mixedListToTriangles :: [[GLuint]] -> [GLuint]
mixedListToTriangles ([]) = []
mixedListToTriangles ((a:b:c:[]):xs) = [a,b,c] ++ (mixedListToTriangles xs)
mixedListToTriangles ((a:b:c:d:[]):xs) = [a,b,c,a,c,d] ++ (mixedListToTriangles xs)

listToTriangles :: (Arrow a) => a [GLuint] [GLuint]
listToTriangles = proc x -> do
                     returnA -< mixedListToTriangles (endBy [(-1)] x)

applyAllIndices = arr (\ (vertices, normals, indices, texCoords, texCoordIndices) ->
                            (applyIndices vertices indices 3,
                             case normals of
                               Nothing -> Nothing
                               Just normals ->
                                   Just (applyIndices normals indices 3),
                             [0..(fromIntegral (length indices))::GLuint],
                             case texCoords of
                               Nothing -> Nothing
                               Just texCoords ->
                                   Just (applyIndices texCoords texCoordIndices 2)
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

                      -- Reasons to apply indices:
                      -- - Per-Face (Texture Coordinates, Normals, Colors)
                      (vertices, normals, indices, texCoords) 
                              <- if (texCoordIndices == [] && texCoords /= Nothing )
                                 then applyAllIndices -< (vertices, normals, indices, texCoords, texCoordIndices)
                                 else returnA -< (vertices, normals, indices, texCoords)

                      normals <- if (normals == Nothing)
                                 then (arr (\(vs, is) ->
                                                let integralIs = [ fromIntegral i | i <- is] in
                                                Just (calculateNormals vs integralIs))) -< (vertices, indices)
                                 else returnA -< normals

                      finalVertices <- (arrIO newArray) -< vertices
                      finalIndices <- (arrIO newArray) -< indices
                      finalTexCoords <- maybeArray -< texCoords
                      finalNormals <- maybeArray -< normals

                      returnA -< X3DGeometryNode X3DIndexedFaceSet { ifsVertices = finalVertices
                                                                   , ifsIndices = finalIndices
                                                                   , ifsIndicesCount = facesCount
                                                                   , ifsTexCoords = finalTexCoords
                                                                   , ifsNormals = finalNormals
                                                                   }

getGeometry :: (ArrowChoice cat, ArrowXml cat, ArrowIO cat) =>
               cat (NTree XNode) X3DGeometryNode
getGeometry = getIndexedFaceSet

data X3DCoordinate = X3DCoordinate
    { cPoint :: [Float]
    }
                  deriving (Show)

getCoordinate = atTag "Coordinate"
                >>>
                proc x -> do                 
                  points <- stringToList <<< getAttrValue "point" -< x
                  returnA -< X3DCoordinate { cPoint = points }

data X3DNormal = X3DNormal
    { nVector :: [Float]
    }
              deriving (Show)

getNormal = (atTag "Normal"
            >>>
            proc x -> do
              vectors <- stringToList <<< getAttrValue "vector" -< x
              returnA -< Just X3DNormal { nVector = vectors }
            ) `orElse` (constA Nothing)

data X3DTextureCoordinate = X3DTextureCoordinate
    { tcPoint :: [Float]
    }
                            deriving (Show)

getTexCoord = (atTag "TextureCoordinate"
               >>>
               proc x -> do
                 points <- stringToList <<< getAttrValue "point" -< x
                 returnA -< Just X3DTextureCoordinate { tcPoint = points }
              ) `orElse` (constA Nothing)
