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
                (1, ThreeOfAKind _)  -> True
                (2, TwoPair _ _)     -> True
                (2, Pair _)          -> True
                _                    -> False
    if bet
        then return pot
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

