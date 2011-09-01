module X3D.Types ( X3DShape (X3DShape)
                 , sAppearance
                 , sGeometry
                 , X3DAppearance (X3DAppearance)
                 , aImageTexture
                 , X3DImageTexture (X3DImageTexture)
                 , itUrl
                 , X3DIndexedFaceSet (X3DIndexedFaceSet)
                 , ifsVertices
                 , ifsIndices
                 , ifsIndicesCount
                 , ifsTexCoords
                 , ifsNormals
                 , X3DCoordinate (X3DCoordinate)
                 , cPoint
                 , X3DNormal (X3DNormal)
                 , nVector
                 , X3DTextureCoordinate (X3DTextureCoordinate)
                 , tcPoint
                 ) where

import Foreign (Ptr)
import Graphics.UI.GLUT

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
