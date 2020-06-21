module ShanYao.Vec3 where

data Vec3 = Vec3 Double Double Double
  deriving (Eq, Show)

-- | Addition.
(>+) :: Vec3 -> Vec3 -> Vec3
(>+) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
infixl 7 >+

-- | Subtraction
(>-) :: Vec3 -> Vec3 -> Vec3
(>-) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
infixl 7 >-

-- | Scalar multiplication.
(>*) :: Vec3 -> Double -> Vec3
(>*) (Vec3 x y z) n = Vec3 (n * x) (n * y) (n * z)
infixl 9 >*

-- | Scalar division.
(>/) :: Vec3 -> Double -> Vec3
(>/) (Vec3 x y z) n = Vec3 (x / n) (y / n) (z / n)
infixl 9 >/

-- | /Cross product/ of two vectors.
(><) :: Vec3 -> Vec3 -> Vec3
(><) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  Vec3 (y1 * z2 - y2 * z1) (z1 * x2 - z2 * x1) (x1 * y2 - x2 * y1)
infixl 8 ><

-- | /Dot product/ of two vectors.
(>.<) :: Vec3 -> Vec3 -> Double
(>.<) (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2
infixl 8 >.<

-- | Length of the vector.
vec3Length :: Vec3 -> Double
vec3Length (Vec3 x y z) = sqrt $ x * x + y * y + z * z

-- | Normalize vector to its unit form.
vec3Unit :: Vec3 -> Vec3
vec3Unit vec3 = vec3 >/ vec3Length vec3
