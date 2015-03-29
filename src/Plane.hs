{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}


module Plane where

import           Record
import           Render

import           BasePrelude               hiding (left, right)
import           Control.Lens
import           Control.Wire              hiding (empty, left, right)
import           Graphics.GLUtil.Camera3D
import           Graphics.Rendering.OpenGL hiding (Color, Normal, Render,
                                            Vertex, light, normal, normalize)
import           Linear

pts :: Int -> [V3 GLfloat]
pts x = V3 <$> [-n, n] <*> [0] <*> [-n, n]
  where
    n = fromIntegral x

vxs :: [ColouredVertex]
vxs = map go (pts 20)
  where
    go p = [r| {pos = p, normal = n, colour = c} |]
    n = V3 0 1 0
    c = V3 0.8 0.2 0.1

mkPlane :: (HasTime t s, Monad m) => Wire s () m a GameObject
mkPlane = arr (const observableState)
  where
    inds = [0, 1, 2, 2, 1, 3]
    observableState = [r| { modelMatrix = camMatrix fpsCamera
                          , vertices = vxs
                          , indices = inds} |]
