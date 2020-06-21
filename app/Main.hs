{-# LANGUAGE OverloadedStrings #-}
module Main where


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
  let r  = (fromIntegral i :: Double) / (fromIntegral nx :: Double)
      g  = (fromIntegral j :: Double) / (fromIntegral ny :: Double)
      b  = 0.2 :: Double
      ir = (floor $ 255.99 * r) :: Int
      ig = (floor $ 255.99 * g) :: Int
      ib = (floor $ 255.99 * b) :: Int
  pure $ show ir <> " " <> show ig <> " " <> show ib
