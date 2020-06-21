module ShanYao.Color3 where

import           ShanYao.Vec3                   ( Vec3(..) )


newtype Color3 = Color3 Vec3
  deriving (Eq, Show)

mkColor3 :: Double -> Double -> Double -> Color3
mkColor3 r g b = Color3 $ Vec3 r g b
