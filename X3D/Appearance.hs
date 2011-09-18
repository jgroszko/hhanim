{-# LANGUAGE Arrows, NoMonomorphismRestriction, ExistentialQuantification #-}

module X3D.Appearance ( X3DAppearance (..)
                      , getAppearance
                      , drawAppearance
                      ) where

import Text.XML.HXT.Core

import Graphics.UI.GLUT

import X3D.LoadUtil

class X3DAppearanceChildNode_ a where
    drawAppearanceChild :: a -> IO ()

data X3DAppearanceChildNode = forall a. (X3DAppearanceChildNode_ a, Show a) => X3DAppearanceChildNode a

instance X3DAppearanceChildNode_ X3DAppearanceChildNode where
    drawAppearanceChild (X3DAppearanceChildNode a) = drawAppearanceChild a

instance Show X3DAppearanceChildNode where
    show (X3DAppearanceChildNode a) = show a

data X3DImageTexture = X3DImageTexture
    { itUrl :: String
    }
                     deriving (Show)

instance X3DAppearanceChildNode_ X3DImageTexture where
    drawAppearanceChild _ = return ()

getImageTexture = atTag "ImageTexture"
                  >>>
                  proc x -> do
                    url <- getAttrValue "url" -< x
                    returnA -< X3DAppearanceChildNode X3DImageTexture { itUrl = url }

data X3DMaterial = X3DMaterial
    { mDiffuse :: Color4 GLfloat
    , mAmbient :: Color4 GLfloat
    , mSpecular :: Color4 GLfloat
    , mEmission :: Color4 GLfloat
    , mShininess :: GLfloat
    }
                 deriving (Show)

instance X3DAppearanceChildNode_ X3DMaterial where
    drawAppearanceChild m = do
      materialDiffuse Front $= (mDiffuse m)
      materialAmbient Front $= (mAmbient m)
      materialSpecular Front $= (mSpecular m)
      materialEmission Front $= (mEmission m)
      materialShininess Front $= (mShininess m)

      return ()

getFloatDefault attr def = getAttrValue attr
                           >>>
                           arr (\x -> if x == ""
                                      then def
                                      else read x)

getColorDefault attr def = getAttrValue attr
                           >>>
                           stringToList
                           >>>
                           arr (\x -> case x of
                                        r:g:b:[] -> Color4 r g b 1.0
                                        _ -> def
                               )

getMaterial = atTag "Material"
              >>>
              proc x -> do
                ambientIntensity <- getFloatDefault "ambientIntensity" 0.2 -< x
                diffuseColor <- getColorDefault "diffuseColor" (Color4 0.8 0.8 0.8 1.0) -< x
                emissiveColor <- getColorDefault "emissiveColor" (Color4 0.0 0.0 0.0 1.0) -< x
                shininess <- getFloatDefault "shininess" 0.2 -< x
                specularColor <- getColorDefault "specularColor" (Color4 0.0 0.0 0.0 1.0) -< x
                -- TODO: Transparency

                ambientColor <- arr (\((Color4 r g b _), ambient) -> Color4 (r*ambient) (g*ambient) (b*ambient) 1.0) -< (diffuseColor, ambientIntensity)

                returnA -< X3DAppearanceChildNode X3DMaterial { mDiffuse = diffuseColor
                                                              , mAmbient = ambientColor
                                                              , mSpecular = specularColor
                                                              , mEmission = emissiveColor
                                                              , mShininess = shininess
                                                              }

getAppearanceChildren = getChildren
                        >>>
                        getImageTexture `orElse` getMaterial

data X3DAppearance = X3DAppearance
    { aChildren :: [X3DAppearanceChildNode]
    }
                     deriving (Show)    

getAppearance = (atTag "Appearance"
                 >>>
                 proc x -> do
                   children <- listA getAppearanceChildren -< x
                   returnA -< Just X3DAppearance { aChildren = children }
                ) `orElse` (constA Nothing)

drawAppearance :: X3DAppearance -> IO ()
drawAppearance a = do
    mapM drawAppearanceChild (aChildren a)
    return ()
