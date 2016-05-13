module GameState where

import Data.Text
import Data.Aeson


data Game
  = Game 
    { tournament_id    :: Text
    , game_id          :: Text
    , round            :: Int
    , bet_index        :: Int
    , small_blind      :: Int
    , current_buy_in   :: Int
    , pot              :: Int 
    , minimum_raise    :: Int
    , dealer           :: Int
    , orbits           :: Int   
    , in_action        :: Int
    , players          :: [ Player ]
    , community_cards  :: [ Card ]
    } deriving (Eq, Show)

instance FromJSON Game where
    parseJSON (Object v) 
        = Game <$> v .: "tournament_id" 
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


data Player 
  = Player
    { player_id  :: Int
    , name       :: Text
    , status     :: Text
    , version    :: Text
    , stack      :: Int
    , bet        :: Int
    , hole_cards :: [ Card ]
    } deriving (Eq, Show)

instance FromJSON Player where
    parseJSON (Object v) 
        = Player <$> v .: "id" 
                 <*> v .: "name"
                 <*> v .: "status"
                 <*> v .: "version"
                 <*> v .: "stack"
                 <*> v .: "bet"
                 <*> v .: "hole_cards"
    parseJSON _  = error "Not an object"

type Rank = Text
type Suit = Text
data Card 
  = Card Rank Suit
    deriving (Eq, Show)

instance FromJSON Card where
    parseJSON (Object v) = Card <$>
                           v .: "rank" <*>
                           v .: "suit"
    parseJSON _  = error "Not an object"

