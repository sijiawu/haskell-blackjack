module Game
(
  Card(..),
  Deck(..),
  Player(..),
  GameStatus(..),
  Game(..),
  cardValues,
  getBasicHandValue,
  getHandValue,
  isSoft17,
  initGame 
)
where 

data Card = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King  
            deriving (Eq, Ord, Show, Enum)

cardValues :: Card -> Int
cardValues Ace   = 11
cardValues Two   = 2
cardValues Three = 3
cardValues Four  = 4
cardValues Five  = 5
cardValues Six   = 6
cardValues Seven = 7
cardValues Eight = 8
cardValues Nine  = 9
cardValues _     = 10

type Deck = [Card]

{- No need to separate the dealer and player into their own modules or different types.  -}
data Player = Player {cardCount :: Int, hand :: [Card], score :: Int, willStay :: Bool}  
    deriving (Show)

{- Depending on how you implement your program, you may not need this. This is here to provide the BlackJack module with 
   information about the game state. But you could just place this inside the Game data-type -}
data GameStatus = PlayerTurn | DealerTurn | GameOverUserBusted | GameOverDealerBusted | GameOverNoOneBusted
    deriving (Eq, Show)


{- All the information you'll need for the game. This can be passed around between BlackJack.hs and Game.hs. 
   The first "player" is the user and the second "Player" is the dealer -}
data Game = Game {user :: Player, dealer :: Player, status :: GameStatus, deck :: Deck}
    deriving (Show)

getBasicHandValue :: [Card] -> Int
getBasicHandValue hand = foldl (\accum x -> accum + cardValues x) 0 hand

getHandValue :: [Card] -> Int
getHandValue hand = if elem Ace hand && getBasicHandValue hand > 21 then getBasicHandValue hand - 10 else getBasicHandValue hand

isSoft17 :: [Card] -> Bool
isSoft17 hand = if elem Ace hand && getBasicHandValue hand == 17 then True else False

initGame :: Deck -> Game
initGame deck = Game {user = user, dealer = dealer, status = PlayerTurn, deck = newDeck}
  where user = Player { cardCount = 2, hand = take 2 deck, score = getHandValue (take 2 deck), willStay = False} 
        dealer = Player { cardCount = 2, hand = take 2 (drop 2 deck), score = getHandValue (take 2 (drop 2 deck)), willStay = False} 
        newDeck = drop 4 deck
