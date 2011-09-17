{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module X3D.Load ( module X3D.Types
                , loadTransform
                ) where

import System.FilePath.Posix
import System.Directory

import Char
import Control.Arrow
import Control.Exception

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

import Foreign (Ptr, newArray)

import Data.List.Split

import Graphics.UI.GLUT

import X3D.Types
import X3D.CalculateNormals
import X3D.Matrices
import X3D.Draw

stringToBool :: (Arrow a) => a String Bool
stringToBool = arr ( \ x -> (compare (map Char.toLower x) "true" == EQ) )

stringToFloat :: (Arrow a) => a String Float
stringToFloat = arr ( \ x -> read x::Float )

stringToList :: (Arrow a) => (Read b) => a String [b]
stringToList = arr (\x -> map read 
                          (split (dropDelims $ dropBlanks $ oneOf ", \n\r\t") x))

mixedListToTriangles :: [[GLuint]] -> [GLuint]
mixedListToTriangles ([]) = []
mixedListToTriangles ((a:b:c:[]):xs) = [a,b,c] ++ (mixedListToTriangles xs)
mixedListToTriangles ((a:b:c:d:[]):xs) = [a,b,c,a,c,d] ++ (mixedListToTriangles xs)

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

applyIndices points index dimensions = foldl (\xs ui ->
                                                  let i = fromIntegral ui in
                                                  xs ++ [ (points !! ((i*dimensions)+n)) | n <- [0..(dimensions-1)] ]
                                             )
                                       []
                                       index                              

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

getTranslation = stringToList
                 >>>
                 arr (\x -> case x of
                              x:y:z:[] -> matTranslation x y z
                              _ -> matIdentity
                     )

-- Easier than calculating the inverse
getNegativeTranslation = stringToList
                         >>>
                         arr (\x -> case x of
                                      x:y:z:[] -> matTranslation (-x) (-y) (-z)
                                      _ -> matIdentity
                             )

getScale = stringToList
           >>>
           arr (\x -> case x of
                        x:y:z:[] -> matScale x y z
                        s:[] -> matScale s s s
                        _ -> matIdentity
               )

getRotation = stringToList
              >>>
              arr (\x -> case x of
                           x:y:z:a:[] -> matRotation a x y z
                           _ -> matIdentity
                  )
-- Easier than calculating the inverse
getNegativeRotation = stringToList
                      >>>
                      arr (\x -> case x of
                                   x:y:z:a:[] -> matRotation (-a) x y z
                                   _ -> matIdentity
                          )

getTransform = atTag "Transform"
               >>>
               proc x -> do
                 t <- getTranslation <<< getAttrValue "translation" -< x
                 c <- getTranslation <<< getAttrValue "center" -< x
                 nc <- getNegativeTranslation <<< getAttrValue "center" -< x
                 r <- getRotation <<< getAttrValue "rotation" -< x
                 sr <- getRotation <<< getAttrValue "scaleOrientation" -< x
                 nsr <- getNegativeRotation <<< getAttrValue "scaleOrientation" -< x
                 s <- getScale <<< getAttrValue "scale" -< x

                 m <- arrIO (\x ->
                             newMatrix RowMajor (map realToFrac (matToFloat x)) :: IO (GLmatrix GLfloat))
                      -< (t `matMult` c `matMult` r `matMult` sr `matMult` s `matMult` nsr `matMult` nc)

                 shape <- getShape -< x

                 returnA -< X3DTransform { tMatrix = m
                                         , tChildren = [X3DChildNode shape] }

getShape = atTag "Shape"
           >>>
           proc x -> do
             appearance <- getAppearance -< x
             geometry <- getIndexedFaceSet -< x
             returnA -< X3DShape { sAppearance = appearance
                                 , sGeometry = geometry }

getAppearance = (atTag "Appearance"
                 >>>
                 proc x -> do
                   imageTexture <- getImageTexture -< x
                   returnA -< Just X3DAppearance { aImageTexture = imageTexture }
                ) `orElse` (constA Nothing)

getImageTexture = (atTag "ImageTexture"
                   >>>
                   proc x -> do
                     url <- getAttrValue "url" -< x
                     returnA -< Just X3DImageTexture { itUrl = url }
                  ) `orElse` (constA Nothing)

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
                              <- if (texCoordIndices == [])
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

loadTransform :: String -> IO [X3DTransform]
loadTransform file = let (dir, filename) = splitFileName file in
                     do
                       olddir <- getCurrentDirectory
                       setCurrentDirectory dir
                       transform <- runX (             
                                          readDocument [ withValidate no 
                                                       , withCurl [] ]
                                          filename
                                          >>>
                                          getTransform
                                         )
                       setCurrentDirectory olddir
                       return transform

