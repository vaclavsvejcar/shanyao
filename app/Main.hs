{-# LANGUAGE OverloadedStrings #-}
module Main where

import           ShanYao.Pixel                  ( Pixel(..) )


main :: IO ()
main = do
  let nx = 200 :: Int
      ny = 100 :: Int
  putStrLn $ "P3\n" <> show nx <> " " <> show ny <> "\n255"
  mapM_ putStrLn (gradient 200 100)


gradient :: Int -> Int -> [String]
gradient nx ny = do
  j <- [ny - 1, ny - 2 .. 0]
  i <- [0 .. nx - 1]
  let (Pixel r g b) = Pixel (fromIntegral i / fromIntegral nx)
                            (fromIntegral j / fromIntegral ny)
                            0.2
  pure
    $  (show . toColor $ r)
    <> " "
    <> (show . toColor $ g)
    <> " "
    <> (show . toColor $ b)
  where toColor x = (floor $ 255.99 * x) :: Int
