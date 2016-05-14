module Hands where

import Data.List (sort)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import GameState (Card(..), Rank(..), Suit(..))


data Hand
  = StraightFlush Card
  | FourOfAKind Card
  | FullHouse Card Card
  | Flush Card
  | Straight Card
  | ThreeOfAKind Card
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
    
matchRank :: Card -> Card -> Bool
matchRank (Card r _) (Card r' _) = r == r'

matchSuit :: Card -> Card -> Bool
matchSuit (Card _ s) (Card _ s') = s == s'


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
findPair player community = 
    let all = player ++ community
        pairs = [ c | c <- all, d <- all, matchRank c d ]
        arePlayer :: Card -> [Card]
        arePlayer a = if a `elem` player then [a] else []
    in case sort pairs of
        a:_ -> Just (arePlayer a, Pair a)
        _   -> Nothing

findHigh :: [Card] -> [Card] -> ([Card], Hand)
findHigh = undefined

