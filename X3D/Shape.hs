{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module X3D.Shape ( getShape
                 , drawShape
                 ) where

import Control.Arrow
import Text.XML.HXT.Core

import {-# SOURCE #-} X3D.ChildNode
import X3D.LoadUtil
import X3D.Geometry
import X3D.Appearance

drawShape :: ChildNode -> IO ()
drawShape shape = do
  case (sAppearance shape) of
    Just appearance -> drawAppearance appearance
    _ -> return ()

  drawGeometry (sGeometry shape)

getShape = atTag "Shape"
           >>>
           proc x -> do
             appearance <- maybeChild getAppearance -< x
             geometry <- getGeometry <<< getChildren -< x
             returnA -< Shape { sAppearance = appearance
                              , sGeometry = geometry }
