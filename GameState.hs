module GameState where

import Data.Text
import Data.Aeson

type Chips = Int
type PlayerID = Int

data Game
  = Game 
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

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine
    | Ten | Jack | Queen | King | Ace 
    deriving (Enum, Eq, Show)
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

data Suit = Clubs | Hearts | Spades | Dimonds
    deriving (Enum, Eq, Show)
instance FromJSON Suit where
    parseJSON (String val)
        = return $
            case val of
                "spades"   -> Spades
                "clubs"    -> Clubs
                "hearts"   -> Hearts
                "dimonds"  -> Dimonds
                _   -> error "Unknown suit"
    parseJSON _  = error "Not a string"


data Card 
  = Card Rank Suit
    deriving (Eq, Show)

instance FromJSON Card where
    parseJSON (Object v) = Card <$>
                           v .: "rank" <*>
                           v .: "suit"
    parseJSON _  = error "Not an object"

