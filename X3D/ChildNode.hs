{-# LANGUAGE ExistentialQuantification #-}
module X3D.ChildNode ( X3DChildNode (..)
                     , X3DChildNode_ (..)
                     ) where

class X3DChildNode_ a where
    draw :: a -> IO ()

data X3DChildNode = forall a. (X3DChildNode_ a, Show a) => X3DChildNode a

instance X3DChildNode_ X3DChildNode where
    draw (X3DChildNode node) = draw node

instance Show X3DChildNode where
    show (X3DChildNode a) = show a
