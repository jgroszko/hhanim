{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module X3D.Shape ( getShape
                 , X3DShape(..)
                 ) where

import Control.Arrow
import Text.XML.HXT.Core

import X3D.ChildNode
import X3D.LoadUtil
import X3D.Geometry

data X3DShape = X3DShape     { sAppearance :: Maybe X3DAppearance
                             , sGeometry :: X3DGeometryNode
                             }
                deriving (Show)

data X3DAppearance = X3DAppearance
    { aImageTexture :: Maybe X3DImageTexture
    }
                     deriving (Show)    

data X3DImageTexture = X3DImageTexture
    { itUrl :: String
    }
                     deriving (Show)

drawShape :: X3DShape -> IO ()
drawShape shape = do
  drawGeometry (sGeometry shape)

instance X3DChildNode_ X3DShape where
    draw = drawShape

getShape = atTag "Shape"
           >>>
           proc x -> do
             appearance <- getAppearance -< x
             geometry <- getGeometry -< x
             returnA -< X3DChildNode X3DShape { sAppearance = appearance
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
