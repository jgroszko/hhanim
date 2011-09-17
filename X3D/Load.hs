{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module X3D.Load ( X3DChildNode (..)
                , draw
                , loadTransform
                ) where

import System.FilePath.Posix
import System.Directory

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

import X3D.Grouping

loadTransform :: String -> IO [X3DChildNode]
loadTransform file = let (dir, filename) = splitFileName file in
                     do
                       olddir <- getCurrentDirectory
                       setCurrentDirectory dir
                       transform <- runX (             
                                          readDocument [ withValidate no 
                                                       , withCurl [] ]
                                          filename
                                          >>>
                                          getTransform
                                         )
                       setCurrentDirectory olddir
                       return transform
