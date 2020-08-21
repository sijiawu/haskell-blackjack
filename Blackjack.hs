import Shuffle
import Game
import Text.Printf

{- IO action for user to decide whether to hit or stay -}
hitOrStay :: IO Int
hitOrStay = do 
    putStrLn "What would you like to do?"
    putStrLn "(1) Hit"
    putStrLn "(2) Stay"
    {- Remember getLine returns a string -}
    optStr <- getLine 
    {- This converts the string into integer -}
    let optionInt = read optStr :: Int 
    return optionInt 

{- IO action for user to decide whether to keep playing -}
newGameOrNaw :: IO Int
newGameOrNaw = do 
    putStrLn "Would you like to play another round?"
    putStrLn "(1) Yah!"
    putStrLn "(2) Naw!"
    {- Remember getLine returns a string -}
    optStr <- getLine 
    {- This converts the string into integer -}
    let optionInt = read optStr :: Int 
    return optionInt 

{-  This function is recursively called and the new gameState is passed in after each turn -}
gameLoop :: Game -> IO () 
gameLoop thisGame = do 
  let stat = Game.status thisGame
  let user = Game.user thisGame
  let dealer = Game.dealer thisGame
  let deck = Game.deck thisGame
  -- printf "Current status: %s \n" (show $ stat)
  -- putStrLn . show $ thisGame
  let userHand = hand user
  let dealerHand = hand dealer
  let userScore = score user
  let dealerScore = score dealer

  let keepPlayingQuestion = newGameOrNaw

  case stat of 
      PlayerTurn -> do
        putStrLn "Your Turn! "
        printf "Your cards: %s   Your score: %s \nDealer's facing-up card: %s \n" (show $ userHand) (show $ userScore) (show $ tail (dealerHand))

        let question = hitOrStay
        userInput <- question
        case userInput of 
          1 -> do 
            let newCardCount = cardCount user + 1
            let newCard = take 1 deck
            let newDeck = drop 1 deck
            printf "You have chosen to hit, here goes -- %s!\n" (show $ head newCard)
            let newHand = hand user ++ newCard
            printf "New hand: %s \n" (show $ newHand) 
            let newScore = getHandValue newHand
            printf "New score: %s \n" (show $ newScore)


            let newStatus = if newScore > 21 then GameOverUserBusted else if willStay dealer then PlayerTurn else DealerTurn 

            let user = Player { cardCount = newCardCount, hand = newHand, score = newScore, willStay = False }
            let next = Game { user = user, dealer = dealer, status = newStatus, deck = newDeck }

            gameLoop next
          2 -> do 
            let newStatus = if willStay dealer then GameOverNoOneBusted else DealerTurn

            let user = Player { cardCount = cardCount user, hand = userHand, score = userScore, willStay = True }
            let next = Game { user = user, dealer = dealer, status = newStatus, deck = deck }
            gameLoop next     

      DealerTurn -> do
        putStrLn "Dealer's Turn! "
        let dealerDecision = if (getHandValue dealerHand <= 16 || isSoft17 dealerHand) then 1 else 2
        case dealerDecision of 
          1 -> do 
            let newCardCount = cardCount dealer + 1
            let newCard = take 1 deck
            let newDeck = drop 1 deck
            printf "Dealer has chosen to hit, here goes -- %s!\n" (show $ head newCard)
            let newHand = hand dealer ++ newCard
            let newScore = getHandValue newHand

            let newStatus = if newScore > 21 then GameOverDealerBusted else if willStay user then DealerTurn else PlayerTurn 

            let dealer = Player { cardCount = newCardCount, hand = newHand, score = newScore, willStay = False }
            let next = Game { user = user, dealer = dealer, status = newStatus, deck = newDeck }

            gameLoop next
          2 -> do 
            putStrLn "Dealer has chosen to stay"
            let newStatus = if willStay user then GameOverNoOneBusted else PlayerTurn

            let dealer = Player { cardCount = cardCount dealer, hand = dealerHand, score = dealerScore, willStay = True }
            let next = Game { user = user, dealer = dealer, status = newStatus, deck = deck }
            gameLoop next     


      GameOverUserBusted -> do 
        putStrLn "Game Over!! You busted! "
        printf "Your cards: %s   Your score: %s \n" (show $ hand user) (show $ userScore)
        printf "Dealer's cards: %s   Dealer's score: %s \n" (show $ hand dealer) (show $ dealerScore)
        newUserInput <- keepPlayingQuestion

        case newUserInput of 
          1 -> do 
            nextDeck <- initDeck
            let nextGame = initGame nextDeck
            gameLoop nextGame
          2 -> return ()

      GameOverDealerBusted -> do
        putStrLn "Game Over!! Dealer busted! "
        printf "Your cards: %s   Your score: %s \n" (show $ hand user) (show $ userScore)
        printf "Dealer's cards: %s   Dealer's score: %s \n" (show $ hand dealer) (show $ dealerScore)
        newUserInput <- keepPlayingQuestion

        case newUserInput of 
          1 -> do 
            nextDeck <- initDeck
            let nextGame = initGame nextDeck
            gameLoop nextGame
          2 -> return ()

      GameOverNoOneBusted -> do
        putStrLn "Game Over!! No one busted! "
        printf "Your cards: %s   Your score: %s \n" (show $ hand user) (show $ userScore)
        printf "Dealer's cards: %s   Dealer's score: %s \n" (show $ hand dealer) (show $ dealerScore)
        newUserInput <- keepPlayingQuestion

        case newUserInput of 
          1 -> do 
            nextDeck <- initDeck
            let nextGame = initGame nextDeck
            gameLoop nextGame
          2 -> return ()


{- Initializes 52 card deck with 4x of each Card -}
initDeck :: IO Deck
initDeck = do
    deck <- shuffle $ [Ace .. King] ++ [Ace .. King] ++ [Ace .. King] ++ [Ace .. King]
    let shuffledDeck = deck :: Deck
    return shuffledDeck


main :: IO () 
main = do 
    putStrLn "Welcome to the game of BlackJack!"

    deck <- initDeck
    let game = initGame deck
    gameLoop game