module ShanYao.Image
  ( Image(..)
  )
where

import qualified Data.ByteString.Lazy          as BL

class Image i where
  renderImage :: i -> BL.ByteString
