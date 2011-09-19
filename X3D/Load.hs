{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module X3D.Load ( X3DChildNode (..)
                , draw
                , loadChildNode
                ) where

import System.FilePath.Posix
import System.Directory

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

import X3D.Grouping
import X3D.HAnim
import X3D.LoadUtil

loadChildNode :: String -> IO [X3DChildNode]
loadChildNode file = let (dir, filename) = splitFileName file in
                     do
                       olddir <- getCurrentDirectory
                       setCurrentDirectory dir
                       node <- runX (
                                     readDocument [ withValidate no 
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
                       setCurrentDirectory olddir
                       return node
