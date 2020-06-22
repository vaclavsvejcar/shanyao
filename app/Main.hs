{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy          as BS
import           ShanYao.Color3                 ( Color3(..) )
import           ShanYao.Image                  ( Image(..) )
import           ShanYao.Image.PPM              ( PPM(..) )
import           ShanYao.Ray                    ( Ray(..) )
import           ShanYao.Vec3                   ( Vec3(..)
                                                , vec3Unit
                                                , (>*)
                                                , (>+)
                                                , (>-)
                                                , (>/)
                                                )


main :: IO ()
main = do
  let
    ratio  = 16 / 9
    width  = 384
    height = (floor $ fromIntegral width / ratio) :: Int
    image  = PPM { ppmWidth  = width
                 , ppmHeight = height
                 , ppmData   = gradient ratio width
                 }
  BS.putStr $ renderImage image


gradient :: Double -> Int -> [Color3]
gradient ratio imageWidth = do
  let imageHeight    = (floor $ fromIntegral imageWidth / ratio) :: Int
      viewportHeight = 2
      viewportWidth  = ratio * viewportHeight
      focalLength    = 1.0
      origin         = Vec3 0 0 0
      horizontal     = Vec3 viewportWidth 0 0
      vertical       = Vec3 0 viewportHeight 0
      lowerLeftCorner =
        origin >- (horizontal >/ 2) >- (vertical >/ 2) >- Vec3 0 0 focalLength

  j <- [imageHeight - 1, imageHeight - 2 .. 0]
  i <- [0 .. imageWidth - 1]

  let
    u   = fromIntegral i / fromIntegral (imageWidth - 1)
    v   = fromIntegral j / fromIntegral (imageHeight - 1)
    ray = Ray
      origin
      (lowerLeftCorner >+ (horizontal >* u) >+ (vertical >* v) >- origin)

  pure $ rayColor ray


rayColor :: Ray -> Color3
rayColor (Ray _ direction) = color
 where
  (Vec3 _ y _) = vec3Unit direction
  t            = 0.5 * (y + 1.0)
  color        = Color3 $ Vec3 1 1 1 >* (1 - t) >+ Vec3 0.5 0.7 1 >* t
