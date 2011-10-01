{-# LANGUAGE Arrows, NoMonomorphismRestriction, ExistentialQuantification #-}

module X3D.Appearance ( Appearance(..)
                      , getAppearance
                      , drawAppearance
                      ) where

import Text.XML.HXT.Core

import Graphics.UI.GLUT

import X3D.LoadUtil
import X3D.Node

data AppearanceChild =
                     Material { mDiffuse :: Color4 GLfloat
                              , mAmbient :: Color4 GLfloat
                              , mSpecular :: Color4 GLfloat
                              , mEmission :: Color4 GLfloat
                              , mShininess :: GLfloat
                              , mName :: String }
                     | ImageTexture { itUrl :: String
                                    , itName :: String }
                       deriving (Show)

drawImageTexture it = do
  return ()

getImageTexture = atTag "ImageTexture"
                  >>>
                  proc x -> do
                    url <- getAttrValue "url" -< x

                    name <- getAttrValue "DEF" -< x

                    returnA -< ImageTexture { itUrl = url 
                                            , itName = name }

drawMaterial m = do
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

getMaterial :: IOSLA (XIOState NodeDict) XmlTree AppearanceChild
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

                name <- getAttrValue "DEF" -< x

                returnA -< Material { mDiffuse = diffuseColor
                                    , mAmbient = ambientColor
                                    , mSpecular = specularColor
                                    , mEmission = emissiveColor
                                    , mShininess = shininess
                                    , mName = name }

getAppearanceChildren :: IOSLA (XIOState NodeDict) XmlTree AppearanceChild
getAppearanceChildren = getChildren
                        >>>
                        (getImageTexture <+> getMaterial)

data Appearance = Appearance
    { aChildren :: [AppearanceChild] }
    deriving (Show) 

getAppearance = atTag "Appearance"
                >>>
                proc x -> do
                   children <- listA getAppearanceChildren -< x
                   returnA -< Appearance { aChildren = children }

drawAppearanceChild a = case a of
                          ImageTexture _ _ -> drawImageTexture a
                          Material _ _ _ _ _ _ -> drawMaterial a

drawAppearance :: Appearance -> IO ()
drawAppearance a = do
    mapM drawAppearanceChild (aChildren a)
    return ()
