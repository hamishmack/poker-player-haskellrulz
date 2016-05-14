module Hands where

import Data.List
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))
import GameState (Card(..), Rank(..), Suit(..))


data Hand
  = StraightFlush Card
  | FourOfAKind Rank
  | FullHouse Rank Rank
  | Flush Card
  | Straight Card
  | ThreeOfAKind Rank
  | TwoPair Rank Rank
  | Pair Rank
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

arePlayerCards :: [Card] -> [Card] -> [Card]
arePlayerCards player cs = [ c | c <- cs, c `elem` player ]


findStraightFlush :: [Card] -> [Card] -> Maybe ([Card], Hand)
findStraightFlush _ _ = Nothing

findFour :: [Card] -> [Card] -> Maybe ([Card], Hand)
findFour player community = 
    let cards = reverse $ sort $ player ++ community
        groups = [ c | c <- groupBy matchRank cards, length c >= 4 ]
    in case (reverse $ sort groups) of
        a:_ -> Just (arePlayerCards player a, FourOfAKind $ rank $ head a)
        _   -> Nothing

findFullHouse :: [Card] -> [Card] -> Maybe ([Card], Hand)
findFullHouse player community = 
    let cards = reverse $ sort $ player ++ community
        threes = [ c | c <- groupBy matchRank cards, length c == 3 ]
        fullhouses = [ (t, c) | t <- threes, c <- groupBy matchRank (cards \\ t), length c == 2 ]
    in case (reverse $ sort fullhouses) of
        (t,c):_ -> Just (arePlayerCards player (t ++ c), FullHouse (rank $ head t) (rank $ head c))
        _   -> Nothing


findFlush :: [Card] -> [Card] -> Maybe ([Card], Hand)
findFlush _ _ = Nothing

-- |
-- >>> (findStraight [] [Card Two Hearts])
-- Nothing
-- >>> (findStraight [] [Card Three Hearts, Card Four Hearts, Card Five Hearts, Card Six Hearts, Card Seven Hearts])
-- Just ([],Straight (Card {rank = Seven, suit = Hearts}))
-- >>> (findStraight [] [Card Two Hearts, Card Four Hearts, Card Five Hearts, Card Six Hearts, Card Seven Hearts])
-- Nothing

findStraight :: [Card] -> [Card] -> Maybe ([Card], Hand)
findStraight holeCards communityCards = 
  -- TODO : This needs to handle straights where an Ace is the low card
  -- TODO : This needs to return which of my cards were used
  let
    cards = holeCards ++ communityCards

    sortedCards = map head . groupBy matchRank . sortOn rank $ cards

    listOfStraitsF :: [Card] -> [[Card]] -> [Card] -> [[Card]]
    listOfStraitsF currentStrait acc remaining =
      case remaining of
        [] -> currentStrait : acc
        (x : xs) ->
          if (fromEnum . rank $ x) == (fromEnum . rank $ (head currentStrait)) + 1
          then listOfStraitsF (x : currentStrait) acc xs
          else listOfStraitsF [x] (currentStrait : acc) xs


    listOfStraits = listOfStraitsF [head sortedCards] [] (tail sortedCards)

    longestStrait = last . sortOn length $ listOfStraits


  in
    if length longestStrait < 5
    then Nothing
    else Just ([], Straight (head longestStrait))



findThree :: [Card] -> [Card] -> Maybe ([Card], Hand)
findThree player community = 
    let cards = reverse $ sort $ player ++ community
        pairs = [ c | c <- groupBy matchRank cards, length c >= 3 ]
    in case (reverse $ sort pairs) of
        a:_ -> Just (arePlayerCards player a, ThreeOfAKind $ rank $ head a)
        _   -> Nothing

findTwoPair :: [Card] -> [Card] -> Maybe ([Card], Hand)
findTwoPair player community = 
    let cards = reverse $ sort $ player ++ community
        pairs = [ c | c <- groupBy matchRank cards, length c >= 2 ]
    in case (reverse $ sort pairs) of
        a:b:_ -> Just (arePlayerCards player (a++b), TwoPair (rank $ head a) (rank $ head b))
        _   -> Nothing 

findPair :: [Card] -> [Card] -> Maybe ([Card], Hand)
findPair player community = 
    let cards = reverse $ sort $ player ++ community
        pairs = [ c | c <- groupBy matchRank cards, length c >= 2 ]
    in case (reverse $ sort pairs) of
        a:_ -> Just (arePlayerCards player a, Pair $ rank $ head a)
        _   -> Nothing

findHigh :: [Card] -> [Card] -> ([Card], Hand)
findHigh player community = 
    let cards = reverse $ sort $ player ++ community
        a = head cards
    in (arePlayerCards player [a], High a)
        

