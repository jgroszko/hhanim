{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module X3D.Shape ( getShape
                 , X3DShape (..)
                 ) where

import Control.Arrow
import Text.XML.HXT.Core

import X3D.ChildNode
import X3D.LoadUtil
import X3D.Geometry
import X3D.Appearance

data X3DShape = X3DShape     { sAppearance :: Maybe X3DAppearance
                             , sGeometry :: X3DGeometryNode
                             }
                deriving (Show)

drawShape :: X3DShape -> IO ()
drawShape shape = do
  case (sAppearance shape) of
    Just appearance -> drawAppearance appearance
    _ -> return ()

  drawGeometry (sGeometry shape)

instance X3DChildNode_ X3DShape where
    draw = drawShape

getShape = atTag "Shape"
           >>>
           proc x -> do
             appearance <- maybeChild getAppearance -< x
             geometry <- getGeometry <<< getChildren -< x
             returnA -< X3DChildNode X3DShape { sAppearance = appearance
                                              , sGeometry = geometry }
