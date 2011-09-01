module CalculateNormals ( calculateNormals
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

listToTriples :: [a] -> [[a]]
listToTriples list = [ [ list !! (i*3)
                       , list !! ((i*3)+1)
                       , list !! ((i*3)+2) ]
                       | i <- [0 .. (((length list) `div` 3) - 1)] ]

facesContaining :: Eq a => a -> [a] -> [[a]]
facesContaining index indices = [ face
                                  | face <- (listToTriples indices), (elem index face) ]

triangleNormal :: Num t => [[t]] -> [t]
triangleNormal [[ax,ay,az], [bx, by, bz], [cx, cy, cz]] =
    let xx = (bx-ax)
        xy = (by-ay)
        xz = (bz-az)
        yx = (cx-ax)
        yy = (cy-ay)
        yz = (cz-az)
    in [ ((xy*yz) - (xz*yy))
       , ((xz*yx) - (xx*yz))
       , ((xx*yy) - (xy*yx)) ]
    
faceToVertices :: [t] -> [Int] -> [[t]]
faceToVertices vertices face = [ [ vertices !! (i*3)
                                 , vertices !! ((i*3)+1)
                                 , vertices !! ((i*3)+2) ]
                                 | i <- face ]

calculateNormals :: Floating a => [a] -> [Int] -> [a]
calculateNormals vertices indices = let vs = listToTriples vertices in
                                    (foldl (\ns i ->
                                                ns ++ (unitVector (foldl (\xs x ->
                                                                  (addVector xs (triangleNormal (faceToVertices vertices x)))
                                                             )
                                                       [0.0,0.0,0.0]
                                                       (facesContaining i indices))))
                                     []
                                     [0 .. (((length vertices) `div` 3) - 1)])
