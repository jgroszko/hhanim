module X3D.Types ( X3DTransform (..)
                 , X3DShape (..)
                 , X3DAppearance (..)
                 , X3DImageTexture (..)
                 , X3DIndexedFaceSet (..)
                 , X3DCoordinate (..)
                 , X3DNormal (..)
                 , X3DTextureCoordinate (..)
                 ) where

import Foreign (Ptr)
import Graphics.UI.GLUT

import X3D.Matrices

data X3DTransform = X3DTransform
    { tMatrix :: GLmatrix GLfloat
    , tShapes :: [X3DShape]
    }
                  deriving (Show)

data X3DShape = X3DShape
    { sAppearance :: Maybe X3DAppearance
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
