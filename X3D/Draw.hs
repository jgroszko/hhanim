module X3D.Draw ( drawTransform
                , drawShape
                , drawIndexedFaceSet
                ) where

import Graphics.UI.GLUT

import X3D.Types

drawTransform :: X3DTransform -> IO ()
drawTransform transform = do
  preservingMatrix (do
                     multMatrix (tMatrix transform) 
                     mapM drawShape (tShapes transform)
                   )
  return ()

drawShape :: X3DShape -> IO ()
drawShape shape = do
  drawIndexedFaceSet (sGeometry shape)

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
