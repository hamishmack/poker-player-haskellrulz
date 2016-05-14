module Hands where

import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import GameState (Card(..))


data Hand
  = StraightFlush Card
  | Four Card
  | FullHouse Card Card
  | Flush Card
  | Straight Card
  | Three Card
  | TwoPair Card Card
  | Pair Card
  | High Card
    deriving (Eq, Show)


getHand :: [Card] -> [Card] -> ([Card], Hand)
getHand player community = 
    fromMaybe (findHigh player community) 
        (  findStraightFlush player community
        <|> findFour player community
        <|> findFullHouse player community
        <|> findFlush player community
        <|> findStraight player community
        <|> findThree player community
        <|> findTwoPair player community
        <|> findPair player community)
    


findStraightFlush :: [Card] -> [Card] -> Maybe ([Card], Hand)
findStraightFlush = undefined

findFour :: [Card] -> [Card] -> Maybe ([Card], Hand)
findFour = undefined

findFullHouse :: [Card] -> [Card] -> Maybe ([Card], Hand)
findFullHouse = undefined


findFlush :: [Card] -> [Card] -> Maybe ([Card], Hand)
findFlush = undefined

findStraight :: [Card] -> [Card] -> Maybe ([Card], Hand)
findStraight = undefined


findThree :: [Card] -> [Card] -> Maybe ([Card], Hand)
findThree = undefined

findTwoPair :: [Card] -> [Card] -> Maybe ([Card], Hand)
findTwoPair = undefined 

findPair :: [Card] -> [Card] -> Maybe ([Card], Hand)
findPair = undefined

findHigh :: [Card] -> [Card] -> ([Card], Hand)
findHigh = undefined

