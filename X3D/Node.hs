{-# LANGUAGE Arrows, ExistentialQuantification #-}
module X3D.Node ( X3DNode (..)
                , X3DNode_ (..)
                , NodeDict
                , getX3DNode
                ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.XML.HXT.Core
import Data.Maybe

import X3D.LoadUtil

class X3DNode_ a

data X3DNode = forall a. (X3DNode_ a, Show a) => X3DNode a

instance X3DNode_ X3DNode

instance Show X3DNode where
    show (X3DNode a) = show a

type NodeDict = Map String X3DNode

getX3DNode
  :: String
     -> IOSLA (XIOState (NodeDict)) XmlTree X3DNode
     -> IOSLA (XIOState (NodeDict)) XmlTree X3DNode
getX3DNode tag getObj = atTag tag
                        >>>
                        proc x -> do
                          defName <- getAttrValue "DEF" -< x
                          useName <- getAttrValue "USE" -< x

                          dict <- getUserState -< ()

                          maybeNode <- (if useName /= ""
                                        then (arr (\(dict, useName) -> Map.lookup useName dict) -< (dict, useName))
                                        else returnA -< Nothing)

                          node <- (if isNothing maybeNode
                                   then getObj -< x
                                   else arr fromJust -< maybeNode)

                          changeUserState (\(name, node) dict -> if name /= ""
                                                                 then Map.insert name node dict
                                                                 else dict) -< (defName, node)
              
                          returnA -< node
