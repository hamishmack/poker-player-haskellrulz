{-# LANGUAGE OverloadedStrings #-}
module GameState where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Text
import Data.Aeson
import Test.QuickCheck
import Data.List

-- $setup
-- >>> import Data.Either (isRight)

-- |
-- >>> parseGameState . BS.pack <$> readFile "test/GameStateSample.json"
-- Right (GameState {tournament_id = "550d1d68cd7bd10003000003", game_id = "550da1cb2d909006e90004b1", round = 0, bet_index = 0, small_blind = 10, current_buy_in = 320, pot = 400, minimum_raise = 240, dealer = 1, orbits = 7, in_action = 1, players = [Player {player_id = 0, name = "Albert", status = Active, version = "Default random player", stack = 1010, bet = 320, hole_cards = Nothing},Player {player_id = 1, name = "Bob", status = Active, version = "Default random player", stack = 1590, bet = 80, hole_cards = Just [Card Six Hearts,Card King Spades]},Player {player_id = 2, name = "Chuck", status = Out, version = "Default random player", stack = 0, bet = 0, hole_cards = Nothing}], community_cards = [Card Four Spades,Card Ace Hearts,Card Six Clubs]})

parseGameState :: BS.ByteString -> Either String GameState
parseGameState = eitherDecode

type Chips = Int
type PlayerID = Int

data GameState
  = GameState
    { tournament_id    :: Text
    , game_id          :: Text
    , round            :: Int
    , bet_index        :: Int
    , small_blind      :: Chips
    , current_buy_in   :: Chips
    , pot              :: Chips
    , minimum_raise    :: Chips
    , dealer           :: PlayerID
    , orbits           :: Int   
    , in_action        :: PlayerID
    , players          :: [ Player ]
    , community_cards  :: [ Card ]
    } deriving (Eq, Show)

instance FromJSON GameState where
    parseJSON (Object v) 
        = GameState 
               <$> v .: "tournament_id" 
               <*> v .: "game_id"
               <*> v .: "round"
               <*> v .: "bet_index"
               <*> v .: "small_blind"
               <*> v .: "current_buy_in"
               <*> v .: "pot"
               <*> v .: "minimum_raise"
               <*> v .: "dealer"
               <*> v .: "orbits"
               <*> v .: "in_action"
               <*> v .: "players"
               <*> v .: "community_cards"
    parseJSON _  = error "Not an object"

data Status = Active | Folded | Out
    deriving (Enum, Eq, Show)
instance FromJSON Status where
    parseJSON (String "active") = return Active
    parseJSON (String "folded") = return Folded
    parseJSON (String "out")    = return Out
    parseJSON _ = error "Unknown Player Status"

data Player 
  = Player
    { player_id  :: PlayerID
    , name       :: Text
    , status     :: Status
    , version    :: Text
    , stack      :: Chips
    , bet        :: Chips
    , hole_cards :: Maybe [ Card ]
    } deriving (Eq, Show)

instance FromJSON Player where
    parseJSON (Object v) 
        = Player <$> v .: "id" 
                 <*> v .: "name"
                 <*> v .: "status"
                 <*> v .: "version"
                 <*> v .: "stack"
                 <*> v .: "bet"
                 <*> v .:? "hole_cards"
    parseJSON _  = error "Not an object"

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
    | Ten | Jack | Queen | King | Ace 
    deriving (Enum, Eq, Show, Ord)
instance FromJSON Rank where
    parseJSON (String val)
        = return $
            case val of
                "2" -> Two
                "3" -> Three
                "4" -> Four
                "5" -> Five
                "6" -> Six
                "7" -> Seven
                "8" -> Eight
                "9" -> Nine
                "10" -> Ten
                "J" -> Jack
                "Q" -> Queen
                "K" -> King
                "A" -> Ace
                _   -> error "Unknown rank"
    parseJSON _  = error "Not a string"

data Suit = Clubs | Hearts | Spades | Diamonds
    deriving (Enum, Eq, Show, Ord)
instance FromJSON Suit where
    parseJSON (String val)
        = return $
            case val of
                "spades"   -> Spades
                "clubs"    -> Clubs
                "hearts"   -> Hearts
                "diamonds"  -> Diamonds
                _   -> error "Unknown suit"
    parseJSON _  = error "Not a string"


data Card 
  = Card {rank :: Rank, suit :: Suit}
    deriving (Show, Ord, Eq)

instance FromJSON Card where
    parseJSON (Object v) = Card <$>
                           v .: "rank" <*>
                           v .: "suit"
    parseJSON _  = error "Not an object"

instance Arbitrary Card where
    arbitrary = Card <$> elements [Two .. Ace] <*> elements [Clubs .. Diamonds]

