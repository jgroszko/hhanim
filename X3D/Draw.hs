module X3D.Draw ( draw
                ) where

import Graphics.UI.GLUT

import X3D.Types

drawTransform :: X3DTransform -> IO ()
drawTransform transform = do
  preservingMatrix (do
                     multMatrix (tMatrix transform) 
                     mapM draw (tChildren transform)
                   )
  return ()

instance X3DChildNode_ X3DTransform where
    draw = drawTransform

drawShape :: X3DShape -> IO ()
drawShape shape = do
  drawIndexedFaceSet (sGeometry shape)

instance X3DChildNode_ X3DShape where
    draw = drawShape

drawIndexedFaceSet :: X3DIndexedFaceSet -> IO ()
drawIndexedFaceSet indexedFaceSet =
  let vertices = (ifsVertices indexedFaceSet)
      indices = (ifsIndices indexedFaceSet)
      indicesCount = fromIntegral (ifsIndicesCount indexedFaceSet)
  in do
    clientState VertexArray $= Enabled
    arrayPointer VertexArray $= VertexArrayDescriptor 3 Float 0 vertices

    case (ifsNormals indexedFaceSet) of
      Nothing -> do clientState NormalArray $= Disabled
      Just normals -> do clientState NormalArray $= Enabled
                         arrayPointer NormalArray $= VertexArrayDescriptor 3 Float 0 normals

    case (ifsTexCoords indexedFaceSet) of
      Nothing -> do clientState TextureCoordArray $= Disabled
      Just texCoords -> do clientState TextureCoordArray $= Enabled
                           arrayPointer TextureCoordArray $= VertexArrayDescriptor 2 Float 0 texCoords

    clientState IndexArray $= Enabled
    drawElements Triangles indicesCount UnsignedInt indices
  
    return ()
