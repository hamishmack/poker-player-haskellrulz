module Player (
    betRequest
  , defaultVersion
  , showdown
  , rankHand
) where

import GameState
import Hands


defaultVersion :: String
defaultVersion = "Default Haskell folding player"

-- |
-- >>> betRequest mempty
-- 0
betRequest :: GameState -> IO Int
betRequest _gameState = return 1000


showdown :: GameState -> IO ()
showdown _gameState = return ()




----------------
-- Rank hand
evalCards :: [Card] -> Int
evalCards = undefined

rankHand :: GameState -> Int
rankHand gs =
    let pl = (players gs) !! (in_action gs)
    in maybe 0 evalCards $ hole_cards  pl

