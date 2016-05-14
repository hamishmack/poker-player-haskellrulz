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

type Strategy = Int
    
data Bet = Fold | Raise | Call

betRequest :: GameState -> IO Int
betRequest gameState@GameState{..} = do
    let me = getPlayer gameState
        player_cards = fromMaybe [] (hole_cards me)
        (mine, hand) = getHand player_cards community_cards

        strategy | length community_cards == 0  = 0
                 | pot `div` small_blind < 10   = 1
                 | pot `div` small_blind < 20   = 2 
                 | pot `div` small_blind < 80   = 3 
                 | pot `div` small_blind < 140  = 4 
                 | _                            = 4
             
        play = 
            case strategy of
                0 -> case (length mine, hand) of 
                        (2, Pair r)          -> if r >= Ten then Raise else Call
                        (1, High (Card r _)) -> if r >= Queen then Call else Fold
                        _                    -> Fold
                1 -> case (length mine, hand) of 
                        (_, Straight _ )     -> Raise
                        (_, FullHouse _ _)   -> Raise
                        (_, FourOfAKind _)   -> Raise
                        (1, ThreeOfAKind _)  -> Raise
                        (2, TwoPair _ _)     -> Call
                        (2, Pair _)          -> Call
                        _                    -> Call
                2 -> case (length mine, hand) of 
                        (_, Straight _ )     -> Raise
                        (_, FullHouse _ _)   -> Raise
                        (_, FourOfAKind _)   -> Raise
                        (1, ThreeOfAKind _)  -> Call
                        (2, TwoPair _ _)     -> Call
                        (2, Pair _)          -> Call
                        _                    -> Call
                3 -> case (length mine, hand) of 
                        (_, Straight _ )     -> Raise
                        (_, FullHouse _ _)   -> Raise
                        (_, FourOfAKind _)   -> Raise
                        (1, ThreeOfAKind _)  -> Call
                        (2, TwoPair _ _)     -> Fold
                        (2, Pair _)          -> Fold
                        _                    -> Fold
                4 -> case (length mine, hand) of 
                        (_, Straight _ )     -> Call
                        (_, FullHouse _ _)   -> Call
                        (_, FourOfAKind _)   -> Call
                        (1, ThreeOfAKind _)  -> Fold
                        (2, TwoPair _ _)     -> Fold
                        (2, Pair _)          -> Fold
                        _                    -> Fold
                _                    -> Fold
    case play of
        Raise -> return pot
        Call  -> return (current_buy_in - (bet me))
        Fold  -> return 0


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

