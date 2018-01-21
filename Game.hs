module Game
(
  Game(..),
  Player(..),
  GameStatus(..),
  initGame,
  playerTurn,
  dealerTurn,
  winner
) where

import Cards

data Player = Player Hand
  deriving (Show)
data GameStatus = PlayerTurn | DealerTurn | GameOver | PlayerStayed
  deriving (Eq)

data Game = Game Player Player GameStatus Deck

-- kick off the game on Player's turn by dealing from the top of the deck
initGame :: Deck -> Game
initGame deck@
  (p1:d1:p2:d2:tail) = Game (Player [p1, p2]) (Player [d1, d2]) PlayerTurn tail

-- hit or stay, based on the player's choice
playerTurn :: Game -> Int -> Game
playerTurn game@(Game player@(Player hand) dealer gStatus deck) playerMove
  | (playerMove == 1) = progressGame (hit game)
  | otherwise = progressGame (Game player dealer PlayerStayed deck)

-- hit or stay
dealerTurn :: Game -> Game
dealerTurn game@(Game player@(Player pHand) dealer@(Player dHand) gStatus deck)
  | dealerHits game = progressGame (hit game)
  | otherwise = progressGame game

-- determines whether the dealer will hit
-- Note: Based on the fact that the behavior for a soft 17 is specified, I am
-- assuming that we are abiding by the rule that the dealer's turn ends when
-- they reach a hard 17 or greater. This means that if the dealer has achieved
-- this score, they will stay, even if the player's score is higher. This
-- behavior is intentional.
dealerHits :: Game -> Bool
dealerHits game@(Game player@(Player pHand) dealer@(Player dHand) gStatus deck)
  = score dHand < 17 || (score dHand == 17 && containsAce dHand)

-- hit for either the player or the dealer (depending on whose turn it is)
hit :: Game -> Game
hit game@
  (Game player@(Player pHand) dealer@(Player dHand) gStatus deck@(dHead:dTail))
  | gStatus == PlayerTurn =
    Game (Player (pHand ++ [dHead])) dealer gStatus dTail
  | gStatus == DealerTurn =
    Game player (Player (dHand ++ [dHead])) gStatus dTail

-- evaluate the state of the game and advance the game status if necessary
progressGame :: Game -> Game
progressGame game@
  (Game player@(Player pHand) dealer@(Player dHand) gStatus deck@(dHead:dTail))
-- Per wiki: If all available players bust, the hand ends automatically
-- without the dealer having to play his or her hand.
  | isBust player || (gStatus == DealerTurn && not (dealerHits game))
    = Game player dealer GameOver deck
  | gStatus == PlayerTurn = Game player dealer PlayerTurn deck
  | gStatus == PlayerStayed || gStatus == DealerTurn
    = Game player dealer DealerTurn deck
  | gStatus == DealerTurn = Game player dealer DealerTurn deck

-- return true if the given player's hand is a bust
isBust :: Player -> Bool
isBust player@(Player hand) = score hand > 21

-- determine the winner
winner :: Game -> [Char]
winner game@(Game player@(Player pHand) dealer@(Player dHand) gStatus _)
  | gStatus /= GameOver = error "Function called prematurely."
  | isBust player || (score dHand > score pHand && not (isBust dealer))
    = "Dealer wins!"
  | score dHand == score pHand || (blackjack dHand && blackjack pHand) = "Draw."
  | blackjack dHand = "Dealer wins!"
  | otherwise  = "Player wins!"
