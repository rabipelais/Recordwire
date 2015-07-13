{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Cube
import           Keyboard
import           Plane
import           Record
import           Render
import           Window

import           BasePrelude
import           Control.Lens
import           Control.Wire              hiding (unless)
import           Graphics.GLUtil
import           Graphics.GLUtil.Camera3D
import           Graphics.Rendering.OpenGL hiding (renderer)
import           Graphics.UI.GLFW
import           Linear                    hiding (trace)

setup :: IO (AppInfo -> [GameObject] -> IO ())
setup = do
  clearColor $= Color4 0.3 0.6 0.3 1
  depthFunc $= Just Lequal
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
  return renderer
  where
    renderer i os = vp i (render i os)
    vPos@(V2 px py) = V2 160 120
    vp = withViewport (Position px py)
       . (\(V2 w h) -> Size w h) . subtract vPos
       . view [l|viewport|]

gameObjects :: [Wire (Timed NominalDiffTime ()) () IO a GameObject]
gameObjects = [mkCube, mkCube, mkPlane]

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
          let V2 ww wh = fromIntegral <$> (windowSize ui - V2 160 120)
              mProj :: M44 GLfloat
              mProj = projectionMatrix (deg2rad 30) (ww / wh) 0.01 100
              mCam :: M44 GLfloat
              mCam = camMatrix c
              proj = mProj !*! mCam
              vp = fromIntegral <$> windowSize ui
              info = [r| { cam = mCam
                         , proj = proj
                         , viewport = vp} |]
          draw info objs'
          unless (keysPressed ui ^. contains Key'Escape)
                 (go (moveCamera ui 0 c) session' ws draw)
    cam0 = fpsCamera
main :: IO ()
main = usage >> initGL "Records test" 640 480 >>= gameLoop

usage :: IO ()
usage = putStrLn "Arrow keys to translate, shift+arrow to rotate, esc to exit! Move the mouse!"
