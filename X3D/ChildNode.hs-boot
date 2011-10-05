module X3D.ChildNode where

import Data.Tree.NTree.TypeDefs
import Control.Arrow
import Text.XML.HXT.Core
import Graphics.UI.GLUT

import X3D.Node
import X3D.Appearance
import X3D.Geometry

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


getChildNode
  :: IOSLA
       (XIOState X3D.Node.NodeDict)
       (Data.Tree.NTree.TypeDefs.NTree XNode)
       ChildNode

drawChildNode :: ChildNode -> IO ()
