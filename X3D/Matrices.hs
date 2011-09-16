module X3D.Matrices ( X3DMatrix (..)
                    , matIdentity
                    , matMult
                    , matTranslation
                    , matScale
                    , matRotation
                    , matToFloat
                    ) where

data X3DMatrix = X3DMatrix [[Float]]

instance Show X3DMatrix where
    show (X3DMatrix a) = show a

matIdentity :: X3DMatrix
matIdentity = X3DMatrix [[1.0, 0.0, 0.0, 0.0],
                         [0.0, 1.0, 0.0, 0.0],
                         [0.0, 0.0, 1.0, 0.0],
                         [0.0, 0.0, 0.0, 1.0]]

matMult :: X3DMatrix -> X3DMatrix -> X3DMatrix
matMult (X3DMatrix a) (X3DMatrix b) =
    let size = (length a) - 1
    in X3DMatrix [
            [ (foldr (\n acc -> (((a !! i) !! n) * ((b !! n) !! j)) + acc) 0 [0..size])
              | j <- [0..size] ]
            | i <- [0..size] ]

matTranslation :: Float -> Float -> Float -> X3DMatrix
matTranslation x y z = X3DMatrix [[1.0, 0.0, 0.0, x],
                                  [0.0, 1.0, 0.0, y],
                                  [0.0, 0.0, 1.0, z],
                                  [0.0, 0.0, 0.0, 1.0]]

matScale :: Float -> Float -> Float -> X3DMatrix
matScale x y z = X3DMatrix [[x, 0.0, 0.0, 0.0],
                            [0.0, y, 0.0, 0.0],
                            [0.0, 0.0, z, 0.0],
                            [0.0, 0.0, 0.0, 1.0]]

matRotation :: Float -> Float -> Float -> Float -> X3DMatrix
matRotation a x y z = let len = (sqrt ((x*x) + (y*y) + (z*z)))
                          nx = x / len
                          ny = y / len
                          nz = z / len
                          c = (cos a)
                          s = (sin a)
                          t = 1-c
                      in
                        X3DMatrix [[ ((nx*nx*t)+c),
                                     ((nx*ny*t)+(nz*s)),
                                     ((nx*nz*t)-(ny*s)),
                                     0.0 ],
                                   [ ((ny*nx*t)-(nz*s)),
                                     ((ny*ny*t)+c),
                                     ((ny*nz*t)+(nx*s)),
                                     0.0 ],
                                   [ ((nx*nz*t)+(ny*s)),
                                     ((ny*nz*t)-(nx*s)),
                                     ((nz*nz*t)+c),
                                     0.0 ],
                                   [ 0.0, 0.0, 0.0, 1.0 ]]

matToFloat :: X3DMatrix -> [Float]
matToFloat (X3DMatrix a) = foldr (++) [] a
