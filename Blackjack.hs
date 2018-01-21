import Cards
import Game
import Shuffle

-- prompt the player to hit or stay
hitOrStay :: IO Int
hitOrStay = do
    putStrLn "What would you like to do?"
    putStrLn "(1) Hit"
    putStrLn "(2) Stay"
    optStr <- getLine
    let optionInt = read optStr :: Int
    return optionInt

-- prompt the player to play again
playAgain :: IO Int
playAgain = do
    putStrLn "Play again?"
    putStrLn "(1) Yes"
    putStrLn "(2) No"
    optStr <- getLine
    let optionInt = read optStr :: Int
    return optionInt

-- repeat until the game concludes
gameLoop :: Game -> IO ()
gameLoop gameState@
  (Game player@(Player pHand) dealer@(Player dHand) gStatus deck)
  | gStatus == PlayerTurn = do
      putStrLn "Player's Turn"
      putStrLn $ "Player: " ++ show pHand ++ " (" ++ show (score pHand) ++ ")"
      putStrLn $ "Dealer: " ++ show (head dHand) ++
        " (>" ++ show (score [(head dHand)]) ++ ")\n"
      playerMove <- hitOrStay
      gameLoop (playerTurn gameState playerMove)
  | gStatus == DealerTurn = do
      putStrLn "Dealer's Turn"
      putStrLn $ "Player: " ++ show pHand ++ " (" ++ show (score pHand) ++ ")"
      putStrLn $ "Dealer: " ++ show dHand ++ " (" ++ show (score dHand) ++ ")\n"
      gameLoop (dealerTurn gameState)
  | gStatus == GameOver = do
      putStrLn $ "Game over; " ++  (winner gameState)
      putStrLn $ "Player: " ++ show pHand ++ " (" ++ show (score pHand) ++ ")"
      putStrLn $ "Dealer: " ++ show dHand ++ " (" ++ show (score dHand) ++ ")\n"
      playerChoice <- playAgain
      if (playerChoice == 1)
      then do main
      else return ()
  | otherwise = return ()

main :: IO ()
main = do
  putStrLn "\nWelcome to the game of BlackJack!"
  shuffledDeck <- shuffle getDeck
  let game = initGame shuffledDeck
  gameLoop game
