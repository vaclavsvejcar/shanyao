{-# LANGUAGE OverloadedStrings #-}
module ShanYao.Image.PPMSpec
  ( spec
  )
where

import           ShanYao.Color3                 ( mkColor3 )
import           ShanYao.Image                  ( Image(..) )
import           ShanYao.Image.PPM
import           Test.Hspec


spec :: Spec
spec = do
  describe "renderImage" $ do
    it "renders PPM image into lazy ByteString" $ do
      let sample = PPM { ppmWidth  = 3
                       , ppmHeight = 2
                       , ppmData   = [mkColor3 0 0.5 1, mkColor3 1 0.5 0]
                       }
          expected = mconcat
            ["P3\n", "3 2\n", "255\n", "  0 127 255\n", "255 127   0\n"]
      renderImage sample `shouldBe` expected
