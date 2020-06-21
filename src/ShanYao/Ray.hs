module ShanYao.Ray where

import           ShanYao.Vec3                   ( Vec3(..)
                                                , (>*)
                                                , (>+)
                                                )


data Ray = Ray Vec3 Vec3
  deriving (Eq, Show)


pointAt :: Ray -> Double -> Vec3
pointAt (Ray origin direction) t = origin >+ direction >* t
