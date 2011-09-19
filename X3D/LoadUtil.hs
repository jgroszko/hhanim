{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
module X3D.LoadUtil ( stringToBool
                    , stringToFloat
                    , stringToList
                    , atTag
                    , maybeChild
                    , maybeArray
                    , maybeProperty
                    ) where

import Char
import Data.List.Split
import Text.XML.HXT.Core

import Foreign (newArray)

stringToBool :: (Arrow a) => a String Bool
stringToBool = arr ( \ x -> (compare (map Char.toLower x) "true" == EQ) )

stringToFloat :: (Arrow a) => a String Float
stringToFloat = arr ( \ x -> read x::Float )

fixFloats :: String -> String
fixFloats x = case x of
                '.':_ -> concat ["0",x]
                '-':'.':xs -> concat ["-0.",xs]
                otherwise -> x 

stringToList :: (Arrow a) => (Read b) => a String [b]
stringToList = arr (\x -> map (read . fixFloats)
                          (split (dropDelims $ dropBlanks $ oneOf ", \n\r\t") x))

atTag tag = isElem >>> hasName tag

maybeChild g = (getChildren >>> g >>> (arr Just)) `orElse` (constA Nothing)

maybeArray = arrIO (\x -> case x of
                            Nothing -> do
                              return Nothing
                            Just y -> do
                              arr <- newArray y
                              return (Just arr))

maybeProperty f = arrIO (\x -> case x of
                                 Nothing -> do
                                   return Nothing
                                 Just y -> do
                                   return (Just (f y)))
