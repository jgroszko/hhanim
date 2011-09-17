{-# LANGUAGE ExistentialQuantification, Arrows, NoMonomorphismRestriction #-}
module X3D.Grouping ( X3DChildNode (..)
                    , getTransform
                    , draw
                    ) where

import Control.Arrow
import Text.XML.HXT.Core

import Graphics.UI.GLUT

import X3D.ChildNode
import X3D.LoadUtil
import X3D.Matrices
import X3D.Shape

data X3DTransform = X3DTransform { tMatrix :: GLmatrix GLfloat
                                 , tChildren :: [X3DChildNode]
                                 }
                  deriving (Show)

drawTransform :: X3DTransform -> IO ()
drawTransform transform = do
  preservingMatrix (do
                     multMatrix (tMatrix transform) 
                     mapM draw (tChildren transform)
                   )
  return ()

instance X3DChildNode_ X3DTransform where
    draw = drawTransform

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

getChildNode = getChildren
               >>>
               getTransform `orElse` getShape

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

                 children <- listA getChildNode -< x

                 returnA -< X3DChildNode X3DTransform { tMatrix = m
                                                      , tChildren = children }
