{-# LANGUAGE ExistentialQuantification #-}
module X3D.ChildNode ( ChildNode(..)
                     , getChildNode
                     , drawChildNode
                     ) where

import Text.XML.HXT.Core
import Graphics.UI.GLUT

import X3D.Appearance
import X3D.Geometry
import X3D.Shape
import X3D.Grouping
import X3D.HAnim

data ChildNode =
               Shape { sAppearance :: Maybe Appearance
                     , sGeometry :: Geometry }
               | Group { gChildren :: [ChildNode] }
               | Transform { tMatrix :: GLmatrix GLfloat
                           , tChildren :: [ChildNode] }
               | HAnimHumanoid { hahChildren :: [ChildNode]
                               , hahName :: String }
               | HAnimJoint { hajJoints :: [ChildNode]
                            , hajName :: String }
               | HAnimSegment { hasChildren :: [ChildNode]
                              , hasName :: String }
                 deriving (Show)

getChildNode = getChildren
               >>>
               (getShape <+> getGroup <+> getTransform <+> getHAnimHumanoid <+> getHAnimJoint <+> getHAnimSegment)

drawChildNode :: ChildNode -> IO ()
drawChildNode node = case node of
                       Shape _ _ -> drawShape node
                       Group _ -> drawGroup node
                       Transform _ _ -> drawTransform node
                       HAnimHumanoid _ _ -> drawHAnimHumanoid node
                       HAnimJoint _ _ -> drawHAnimJoint node
                       HAnimSegment _ _ -> drawHAnimSegment node
