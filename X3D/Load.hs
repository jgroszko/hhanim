{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module X3D.Load ( ChildNode (..)
                , loadChildNode
                , drawChildNode
                ) where

import System.FilePath.Posix
import System.Directory
import Data.Map (Map)
import qualified Data.Map as Map

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

import X3D.LoadUtil
import X3D.Node
import X3D.ChildNode

loadChildNode :: String -> IO [ChildNode]
loadChildNode file = let (dir, filename) = splitFileName file in
                     do
                       olddir <- getCurrentDirectory
                       setCurrentDirectory dir
                       node <- runX (withOtherUserState (Map.empty::NodeDict)
                                     (readDocument [ withValidate no 
                                                   , withCurl [] ]
                                      filename
                                      >>>
                                      atTag "/"
                                      >>>
                                      getChildren >>> atTag "X3D"
                                      >>>
                                      getChildren >>> atTag "Scene"
                                      >>>
                                      getChildNode
                                     )
                                    )
                       setCurrentDirectory olddir
                       return node
