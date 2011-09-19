{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module X3D.HAnim ( X3DHAnimHumanoid (..)
                 , getHumanoid
                 , getSegment
                 ) where

import Control.Arrow
import Text.XML.HXT.Core
import X3D.LoadUtil
import X3D.ChildNode
import {-# SOURCE #-} X3D.Grouping (getChildNode)

data X3DHAnimSegment = X3DHAnimSegment
    { hasChildren :: [X3DChildNode]
    }
                     deriving (Show)

getSegment = atTag "HAnimSegment"
             >>>
             proc x -> do
               children <- listA getChildNode -< x
                           
               returnA -< X3DHAnimSegment { hasChildren = children }

drawSegment segment = do
  mapM draw (hasChildren segment)
  return ()

data X3DHAnimJoint = X3DHAnimJoint
    { hajJoints :: [X3DHAnimJoint]
    , hajSegment :: Maybe X3DHAnimSegment
    , hajUSE :: String
    , hajDEF :: String
    }
                     deriving (Show)

getJoint = atTag "HAnimJoint"
           >>>
           proc x -> do
             joints <- listA (getChildren >>> getJoint) -< x
             segment <- maybeChild getSegment -< x

             use <- getAttrValue "USE" -< x
             def <- getAttrValue "DEF" -< x

             returnA -< X3DHAnimJoint { hajJoints = joints
                                      , hajSegment = segment
                                      , hajUSE = use 
                                      , hajDEF = def }

drawJoint joint = do
  case (hajSegment joint) of
    Nothing -> return ()
    Just segment -> drawSegment segment

  mapM drawJoint (hajJoints joint)
  return ()

data X3DHAnimHumanoid = X3DHAnimHumanoid
    { hahJoints :: [X3DHAnimJoint]
    }
                        deriving (Show)

instance X3DChildNode_ X3DHAnimHumanoid where
    draw humanoid = do
      mapM drawJoint (hahJoints humanoid)
      return ()

getHumanoid = atTag "HAnimHumanoid"
              >>>
              proc x -> do
                joints <- listA (getChildren >>> getJoint) -< x
                
                returnA -< X3DChildNode X3DHAnimHumanoid { hahJoints = joints }
