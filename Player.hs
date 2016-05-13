module Player (
    betRequest
  , version
  , showdown
) where

import Data.Aeson (Object)


defaultVersion :: String
defaultVersion = "Default Haskell folding player"

-- |
-- >>> betRequest mempty
-- 0
betRequest :: Object -> IO Int
betRequest gameState = return 0

showdown :: Object -> IO ()
showdown gameState = return ()

