{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module X3D.HAnim ( getHAnimSegment
                 , drawHAnimSegment
                 , getHAnimJoint
                 , drawHAnimJoint
                 , getHAnimHumanoid
                 , drawHAnimHumanoid
                 ) where

import Control.Arrow
import Text.XML.HXT.Core
import X3D.LoadUtil
import {-# SOURCE #-} X3D.ChildNode

drawHAnimSegment s = do
  mapM drawChildNode (hasChildren s)
  return ()

getHAnimSegment = atTag "HAnimSegment"
                  >>>
                  proc x -> do
                    children <- listA getChildNode -< x
                              
                    name <- getAttrValue "DEF" -< x

                    returnA -< HAnimSegment { hasChildren = children
                                            , X3D.ChildNode.hasName = name }

drawHAnimJoint j = do
  mapM drawChildNode (hajJoints j)
  return ()

getHAnimJoint = atTag "HAnimJoint"
                >>>
                proc x -> do
                  joints <- listA getChildNode -< x

                  name <- getAttrValue "DEF" -< x

                  returnA -< HAnimJoint { hajJoints = joints
                                        , hajName = name }

drawHAnimHumanoid h = do
  mapM drawChildNode (hahChildren h)
  return ()

getHAnimHumanoid = atTag "HAnimHumanoid"
                   >>>
                   proc x -> do
                     children <- listA getChildNode -< x

                     name <- getAttrValue "DEF" -< x
                                 
                     returnA -< HAnimHumanoid { hahChildren = children
                                              , hahName = name }
