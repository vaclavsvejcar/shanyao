module ShanYao.Vec3Spec
  ( spec
  )
where

import           ShanYao.Vec3
import           Test.Hspec


spec :: Spec
spec = do
  describe "(|+>)" $ do
    it "should calculate addition of two vectors" $ do
      let vec1     = Vec3 1 0 3
          vec2     = Vec3 (-1) 4 2
          expected = Vec3 0 4 5
      vec1 |+> vec2 `shouldBe` expected


  describe "(|->)" $ do
    it "should calculate subtraction of two vectors" $ do
      let vec1     = Vec3 4 2 (-1)
          vec2     = Vec3 (-1) 2 3
          expected = Vec3 5 0 (-4)
      vec1 |-> vec2 `shouldBe` expected


  describe "(><)" $ do
    it "should calculate cross product of two vectors" $ do
      let vec1     = Vec3 3 (-3) 1
          vec2     = Vec3 (-12) 12 4
          expected = Vec3 (-24) (-24) 0
      vec1 >< vec2 `shouldBe` expected


  describe "(>.<)" $ do
    it "should calculate dot product of two vectors" $ do
      let vec1 = Vec3 6 (-1) 3
          vec2 = Vec3 4 18 (-2)
      vec1 >.< vec2 `shouldBe` 0
