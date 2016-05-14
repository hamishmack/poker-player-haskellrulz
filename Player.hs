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
defaultVersion = "First attempt at a strategy"

betRequest :: GameState -> IO Int
betRequest gameState@GameState{..} = do
    let player_cards = fromMaybe [] (hole_cards $ getPlayer gameState)
        (mine, hand) = getHand player_cards community_cards
        
        bet = 
            case (length mine, hand) of 
                (_, FullHouse _ _)   -> True
                (_, FourOfAKind _)   -> True
                (_, ThreeOfAKind _)  -> True
                (_, TwoPair _ _)     -> True
                (_, Pair _)          -> True
                _               -> False
    if bet
        then pot
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

