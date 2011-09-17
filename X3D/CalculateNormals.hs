module X3D.CalculateNormals ( calculateNormals
                        ) where

unitLength :: (Floating a) => [a] -> a
unitLength [a, b, c] = (sqrt ((a*a) + (b*b) + (c*c)))

unitVector :: (Floating a) => [a] -> [a]
unitVector [a, b, c] = let len = unitLength [a, b, c] in
                       if len == 0.0
                       then [0.0, 0.0, 0.0]
                       else [(a / len), (b / len), (c / len)]

addVector :: (Num t) => [t] -> [t] -> [t]
addVector [ax,ay,az] [bx,by,bz] = [ax+bx,ay+by,az+bz]

triangleNormal :: Floating t => [[t]] -> [t]
triangleNormal [[ax,ay,az], [bx, by, bz], [cx, cy, cz]] =
    let xx = (bx-ax)
        xy = (by-ay)
        xz = (bz-az)
        yx = (cx-ax)
        yy = (cy-ay)
        yz = (cz-az)
    in
      [ ((xy*yz) - (xz*yy))
      , ((xz*yx) - (xx*yz))
      , ((xx*yy) - (xy*yx)) ]
    
normalFacePairs :: Floating a => [a] -> [Int] -> [([Int],[a])]
normalFacePairs vertices indices = [ let is = [ indices !! (i*3)
                                             , indices !! ((i*3)+1)
                                             , indices !! ((i*3)+2)]
                                    in (is, (triangleNormal (faceToVertices vertices is)))
                                    | i <- [0..(((length indices) `div` 3) - 1)]]

pairsToNormals index pairs = [ normal
                               | (face, normal) <- pairs, (elem index face) ]

calculateNormals :: Floating a => [a] -> [Int] -> [a]
calculateNormals vertices indices = let numVertices = (((length vertices) `div` 3) - 1)
                                        nfps = normalFacePairs vertices indices
                                    in
                                      concat [ unitVector (foldr addVector [0.0,0.0,0.0] (pairsToNormals i nfps))
                                               | i <- [0..numVertices] ]
