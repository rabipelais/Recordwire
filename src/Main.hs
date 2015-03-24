{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import           Keyboard
import           Window

import           BasePrelude
import           Control.Lens
import           Control.Wire              hiding (unless)
import           Graphics.GLUtil
import           Graphics.GLUtil.Camera3D
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLFW
import           Linear

setup = do
  clearColor $= Color4 0.3 0.6 0.3 1
  depthFunc $= Just Lequal
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  return renderer
  where
    renderer = undefined

gameObjects = []

gameLoop :: IO UI -> IO ()
gameLoop inputSource = setup >>= go cam0 clockSession_ gameObjects
  where
    go c session wires draw = do
      ui <- inputSource
      clear [ColorBuffer, DepthBuffer]
      (ds, session') <- stepSession session
      result <- mapM (\w -> stepWire w ds $ Right undefined) wires
      let (objE, ws) = unzip result
          objs = sequence objE
      case objs of
        Left _ -> return ()
        Right objs' -> do
          let V2 ww wh = fromIntegral <$> (- V2 160 120)
              mProj :: M44 GLfloat
              mProj = projectionMatrix (deg2rad 30) (ww / wh) 0.01 100
              mCam :: M44 GLfloat
              mCam = camMatrix c
          --     info = undefined
          unless (keysPressed ui ^. contains Key'Escape)
                 (go (moveCamera ui c) session' ws draw)
    cam0 = tilt (-20) $ dolly (V3 0 2 8) fpsCamera

main :: IO ()
main = usage >> initGL "Records test" 640 480 >>= gameLoop

usage :: IO ()
usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, esc to exit!"
