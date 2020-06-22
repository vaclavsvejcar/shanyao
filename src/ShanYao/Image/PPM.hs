{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module ShanYao.Image.PPM where

import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Char8    as CL
import           ShanYao.Color3                 ( Color3(..) )
import           ShanYao.Image                  ( Image(..) )
import           ShanYao.Vec3                   ( Vec3(..) )
import           Text.Printf                    ( printf )



data PPM = PPM
  { ppmWidth  :: Int
  , ppmHeight :: Int
  , ppmData   :: [Color3]
  }
  deriving (Eq, Show)


instance Image PPM where
  renderImage = renderImage'


renderImage' :: PPM -> BL.ByteString
renderImage' PPM {..} = CL.unlines
  [ "P3"
  , (CL.pack . show $ ppmWidth) <> " " <> (CL.pack . show $ ppmHeight)
  , "255"
  , CL.intercalate "\n" $ fmap encodeColor ppmData
  ]
 where
  encodeColor (Color3 (Vec3 r g b)) =
    CL.intercalate " " $ to8BitColor <$> [r, g, b]

to8BitColor :: Double -> BL.ByteString
to8BitColor x = CL.pack $ printf "%3d" ((floor $ 255 * x) :: Int)
