{-# LANGUAGE RecordWildCards #-}
module Player (
    betRequest
  , defaultVersion
  , showdown
  , rankHand
) where

import Data.Maybe (fromMaybe)

import GameState
import Hands


defaultVersion :: String
defaultVersion = "Default Haskell folding player"

betRequest :: GameState -> IO Int
betRequest gameState@GameState{..} = do
    let player_cards = fromMaybe [] (hole_cards $ getPlayer gameState)
        (mine, hand) = getHand player_cards community_cards
        
        potentialbet = 
            case (length mine, hand) of 
                (_, FullHouse _ _)   -> 1000
                (_, FourOfAKind _)   -> 200
                (1, ThreeOfAKind _)  -> 100
                (2, TwoPair _ _)     -> 75
                (2, Pair _)          -> 50
                _               -> 0
    if potentialbet > minimum_raise
        then return potentialbet
        else return 0


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

