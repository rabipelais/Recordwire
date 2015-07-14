{-# LANGUAGE ViewPatterns #-}

module Keyboard (moveCamera) where

import           Data.Foldable            (Foldable, fold, foldMap, foldl')
import           Data.Monoid              (All (..), Any (..))
import qualified Data.Set                 as S
import           Graphics.GLUtil.Camera3D (Camera (..), dolly, pan,
                                           panGlobalRad, roll, tilt, tiltRad)
import           Graphics.UI.GLFW         (Key (..))
import           Linear                   (Conjugate, Epsilon, Quaternion (..),
                                           V2 (..), V3 (..), axisAngle, cross,
                                           dot, normalize, quadrance, rotate,
                                           (^*))
import           Window                   (UI (..))


-- | Evaluate a boolean formula in conjunctive normal form (CNF) by
-- applying the predicate to each atom according to the logic of its
-- nesting in the formula.
cnf :: (Foldable s, Foldable t) => s (t Bool) -> Bool
cnf = getAll . foldMap (All . getAny . foldMap Any)

-- | Perform a left fold over a set of guarded update functions,
-- evaluating the guards left-to-right. For each guard that passes,
-- its associated update function is composed into a final composite
-- update function.
cnfEndo :: (k -> s -> Bool) -> (k -> s -> s) -> [([[k]], a -> a)] -> s -> a -> a
cnfEndo p del = go
  where go [] _ = id
        go ((k,f):fs) s | cnf (fmap (fmap (`p` s)) k) = go fs (delAll k s) . f
                        | otherwise = go fs s
        delAll k s = foldl' (flip del) s (fold k)

-- | Given two points on the plane (the screen), and the size of the plane,
-- project them to a sphere of radius 1.0 around the origin and calculate
-- the corresponding rotation from `p1` to `p2`
arcball ::  V2 Double -> V2 Double -> V2 Int -> Quaternion Double
arcball p1 p2 (fmap fromIntegral -> V2 width height)
    | p1 == p2 = 1
    | otherwise = quat
  where
    va = arcballVector p1 -- Vectors on the arc ball
    vb = arcballVector p2
    angle = acos (min 1.0 (dot va vb))
    axis = cross va vb
    quat = axisAngle axis angle

    arcballVector (fmap realToFrac -> (V2 x y)) =
      normalize $ V3 x' y' z
      where
        x' = (2 * x / width) - 1.0
        y' = negate $ (2 * y / height) - 1.0
        z = if quadrance (V2 x' y') <= 1.0
               then sqrt (1.0 - quadrance (V2 x' y'))
               else 0.0

walkDistance = 2

-- | Rotate the camera around the `target` point while maintaining distance `dist`.
-- This is similar to some third-person RPGs. Note that the camera never changes its
-- orientation around its forward axis.
aroundTarget :: (Conjugate a, Epsilon a, RealFloat a) =>
                a            -- ^ The distance to the target
                -> V2 Double -- ^ Movement vector of the mouse cursor
                -> V3 a      -- ^ Target's 3D coordinates
                -> Camera a -> Camera a
aroundTarget dist (fmap realToFrac -> V2 dx dy) target c@Camera{orientation = o, location = l} = c' {location = pos}
  where
    dx' = (dx * xSpeed * 0.02)
    dy' = (dy * ySpeed * 0.02)
    (xSpeed, ySpeed) = (0.5, 0.5)
    c' = tiltRad (-dy') . panGlobalRad (dx') $ c
    pos = rotate (orientation c') (V3 0 0 dist) + target

-- | Calculates the quaternions Euler angles in the Tait-Bryan convention,
-- also know as the `NASA` convention.
quat2Euler :: (RealFloat a) =>  Quaternion a -> (a, a, a)
quat2Euler (Quaternion q0 (V3 q1 q2 q3)) = (x, y, z)
  where
    x = atan2 (2 * (q0*q1 + q2*q3)) (1 - 2* (q1*q1 + q2*q2))
    y = asin (2 * (q0*q2 - q3*q1))
    z = atan2 (2 * (q0*q3 + q1*q2)) (1 - 2* (q2*q2 + q3*q3))

-- | Translate and rotate a 'Camera' based on 'UI' input.
moveCamera :: (Conjugate a, Epsilon a, RealFloat a) => UI -> V3 a -> Camera a -> Camera a
moveCamera ui focus = aroundTarget 30 diffMouse focus .
                      cnfEndo S.member S.delete
                      [ ([shift, ctrl, [Key'Left]], roll na)
                       , ([shift, ctrl, [Key'Right]], roll pa)
                       , ([shift, [Key'Left]], pan pa)
                       , ([shift, [Key'Right]], pan na)
                       , ([shift, [Key'Up]], tilt pa)
                       , ([shift, [Key'Down]], tilt na)
                       , ([[Key'Left]], dolly (V3 np 0 0))
                       , ([[Key'Right]], dolly (V3 pp 0 0))
                       , ([[Key'Up]], dolly (V3 0 0 np))
                       , ([[Key'Down]], dolly (V3 0 0 pp))
                       , ([[Key'PageUp]], dolly (V3 0 pp 0))
                       , ([[Key'PageDown]], dolly (V3 0 np 0)) ]
                       (keysPressed ui)
  where shift = [Key'LeftShift, Key'RightShift]
        ctrl = [Key'LeftControl, Key'RightControl]

        diffMouse = mousePos ui - lastMousePos ui
        -- Normalize speeds to 60Hz update
        timeScale = 0.1 * 60
        pp = 0.08 * timeScale -- 1D speed
        np = negate pp
        pa = 0.2 * timeScale    -- angular step
        na = negate pa
