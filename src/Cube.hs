{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Cube where

import           Record
import           Render

import           BasePrelude               hiding (left, right)
import           Control.Lens
import           Control.Wire              hiding (empty, left, right)
import           Graphics.GLUtil.Camera3D
import           Graphics.Rendering.OpenGL hiding (Color, Normal, Render,
                                            Vertex, light, normal, normalize)
import           Linear

-- The 2D corners of a square.
square :: [V2 GLfloat]
square = V2 <$> [-1, 1] <*> [1, -1]

-- The 3D faces of a cube.
front,back,left,right,top,bottom :: [V3 GLfloat]
front  = map (\(V2 x y) -> V3 x y 1) square
back   = map (\(V2 x y) -> V3 (-x) y (-1)) square
left   = map (\(V2 z y) -> V3 (-1) y z) square
right  = map (\(V2 z y) -> V3 1 y (-z)) square
top    = map (\(V2 x z) -> V3 x 1 (-z)) square
bottom = map (\(V2 x z) -> V3 x (-1) z) square

-- Cube face vertices paired with normal vectors.
pts :: [Vertex]
pts = fold [ map (setNorm z) front
           , map (setNorm $ -z) back
           , map (setNorm $ -x) left
           , map (setNorm x) right
           , map (setNorm y) top
           , map (setNorm $ -y) bottom ]
  where
    [x,y,z] = basis
    setNorm v p = [r| {pos = p, normal = v} |]

-- Color the front vertices a dark blue, the back a light beige.
colourise :: Vertex -> ColouredVertex
colourise pt = [r| {pos = p, normal = n, colour = c} |]
  where
    p = pt^.[l|pos|]
    n = pt^.[l|normal|]
    c | p^._z > 0 = V3 8.235294e-2 0.20392157 0.3137255
      | otherwise = V3 0.95686275 0.8392157 0.7372549

-- Indices into the vertex array for each face.
inds :: [Word32]
inds = take 36 $ foldMap (flip map faceInds . (+)) [0, 4..]
  where
    faceInds = [0, 1, 2, 2, 1, 3]

mkCube :: (HasTime t s, Monad m) => Wire s () m a GameObject
mkCube = arr (const observableState)
  where
    vxs = map colourise pts
    observableState = [r| { modelMatrix = camMatrix fpsCamera
                          , vertices = vxs
                          , indices = inds} |]
