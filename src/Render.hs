{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Render where

import           Graphics.RecordGL
import           Record
import           Record.Introspection
import           Record.Types

import           BasePrelude
import           Control.Lens              hiding (indices)
import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL hiding (Color, Normal, Render,
                                            Vertex, light, normal, normalize)
import           Linear
import           System.FilePath           ((</>))

type Vertex = [r| { pos :: V3 GLfloat
                  , normal :: V3 GLfloat} |]

type ColouredVertex = [r| { pos :: V3 GLfloat
                          , normal :: V3 GLfloat
                          , colour :: V3 GLfloat} |]
type ColouredVertices = [ColouredVertex]

type ObservableState = [r| {modelMatrix :: M44 GLfloat, vertices :: ColouredVertices, indices :: [Word32]}|]


type CamInfo i m = (Field' "cam" i m, Field' "proj" i m, HasFieldNames i, HasFieldGLTypes i, SetUniformFields i)

type AppInfo = [r| { cam :: M44 GLfloat
                   , proj :: M44 GLfloat
                   , viewport :: V2 GLsizei} |]

type GameObject = ObservableState


render :: ( Field' "vertices" o ColouredVertices, Field' "indices" o [Word32]
          , CamInfo i (M44 GLfloat))
       => i -> [o] -> IO ()
render cam = mapM_ (render' cam)

render' :: ( Field' "vertices" o ColouredVertices, Field' "indices" o [Word32]
           , CamInfo i (M44 GLfloat))
         => i -> o -> IO ()
render' cam ob = do
  s <- simpleShaderProgram ("res" </> "poly.vert") ("res" </> "poly.frag")
  vb <- bufferVertices (ob ^. [l|vertices|])
  eb <- makeBuffer ElementArrayBuffer (ob^. [l|indices|])
  vao <- makeVAO $ do
    currentProgram $= Just (program s)
    setUniforms s light
    enableVertices' s vb
    bindVertices vb
    bindBuffer ElementArrayBuffer $= Just eb
  let ss = setSomeUniforms s
  withVAO vao $ do
    currentProgram $= Just (program s)
    ss cam
    drawIndexedTris 12
  deleteVertices vb
  deleteObjectName eb
  deleteVAO vao
  where
    light :: [r| {lightDir :: V3 GLfloat} |]
    light = [r| {lightDir = normalize (V3 0 0 1)} |]
