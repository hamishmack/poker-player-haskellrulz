module Player (
    betRequest
  , version
  , showdown
) where

import Data.Aeson (Object)


version :: String
version = "Default Haskell folding player"

-- |
-- >>> betRequest mempty
-- 0
betRequest :: Object -> IO Int
betRequest gameState = return 0

showdown :: Object -> IO ()
showdown gameState = return ()

