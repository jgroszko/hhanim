{-# LANGUAGE ExistentialQuantification #-}
module X3D.Types ( X3DChildNode_(..)
                 , X3DTransform(..)
                 , X3DShape(..)
                 , X3DChildNode (..)
                 , X3DAppearance (..)
                 , X3DImageTexture (..)
                 , X3DIndexedFaceSet (..)
                 , X3DCoordinate (..)
                 , X3DNormal (..)
                 , X3DTextureCoordinate (..)
                 , show
                 ) where

import Foreign (Ptr)
import Graphics.UI.GLUT

import X3D.Matrices

class X3DChildNode_ a where
    draw :: a -> IO ()

data X3DChildNode = forall a. (X3DChildNode_ a, Show a) => X3DChildNode a

instance X3DChildNode_ X3DChildNode where
    draw (X3DChildNode node) = draw node

instance Show X3DChildNode where
    show (X3DChildNode a) = show a

data X3DTransform = X3DTransform { tMatrix :: GLmatrix GLfloat
                                 , tChildren :: [X3DChildNode]
                                 }
                  deriving (Show)

data X3DShape = X3DShape     { sAppearance :: Maybe X3DAppearance
                             , sGeometry :: X3DIndexedFaceSet
                             }
                deriving (Show)

data X3DAppearance = X3DAppearance
    { aImageTexture :: Maybe X3DImageTexture
    }
                     deriving (Show)    

data X3DImageTexture = X3DImageTexture
    { itUrl :: String
    }
                     deriving (Show)

data X3DIndexedFaceSet = X3DIndexedFaceSet
    { ifsVertices :: Ptr Float
    , ifsIndices :: Ptr GLuint
    , ifsIndicesCount :: Int
    , ifsTexCoords :: Maybe (Ptr Float)
    , ifsNormals :: Maybe (Ptr Float)
    }
                      deriving (Show)

data X3DCoordinate = X3DCoordinate
    { cPoint :: [Float]
    }
                  deriving (Show)

data X3DNormal = X3DNormal
    { nVector :: [Float]
    }
              deriving (Show)

data X3DTextureCoordinate = X3DTextureCoordinate
    { tcPoint :: [Float]
    }
                            deriving (Show)
