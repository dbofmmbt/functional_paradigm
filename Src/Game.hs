module Src.Game where

import Data.Maybe
import Src.Bucket
import qualified Src.Bucket as Bucket (new)
import Src.GameState

type Target = Volume

data Game = Game {state :: [GameState], target :: Volume}

instance Show Game where
  show (Game state _) = concat $ map ((++ "\n") . show) state

new :: Volume -> Volume -> Target -> Maybe Game
new l r t = Just $ Game {state = [initialState], target = t}
  where
    initialState = G (Bucket.new l) (Bucket.new r)

currentState :: Game -> GameState
currentState = head . state

nextGame :: Game -> GameState -> Maybe Game
nextGame g s 
  | alreadyVisited g s = Nothing
  | otherwise = Just g {state = (s : (state g))}

targetReached :: Game -> Bool
targetReached g = target g == (volume $ currentState g)

nextGames :: Game -> [Game]
nextGames g = catMaybes $ map (nextGame g) (nextStates (currentState g))

alreadyVisited :: Game -> GameState -> Bool
alreadyVisited g gs = gs `elem` (state g)

play :: Game -> Game --WIP
play = head . playAux

playAux :: Game -> [Game]
playAux g
  | targetReached g = [g]
  | otherwise = concat $ map playAux (nextGames g)
