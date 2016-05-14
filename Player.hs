module Player (
    betRequest
  , defaultVersion
  , showdown
) where

import Data.Aeson (Object)
import GameState


defaultVersion :: String
defaultVersion = "Default Haskell folding player"

-- |
-- >>> betRequest mempty
-- 0
betRequest :: GameState -> IO Int
betRequest gameState = return 1000

showdown :: GameState -> IO ()
showdown gameState = return ()

