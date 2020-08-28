module Src.GameState where

import Data.Tuple
import Src.Bucket

data GameState = G {left :: Bucket, right :: Bucket} deriving (Eq)

instance Show GameState where
  show (G l r) = show l ++ " | " ++ show r 

volume :: GameState -> Int
volume (G l r) = (current l) + (current r)

transferLeftToRight :: GameState -> GameState
transferLeftToRight (G l r) = uncurry G $ transfer l r

transferRightToLeft :: GameState -> GameState
transferRightToLeft (G l r) = uncurry G $ swap $ transfer r l

fillLeft :: GameState -> GameState
fillLeft (G l r) = (G (fill l) r)

fillRight :: GameState -> GameState
fillRight (G l r) = (G l (fill r))

emptyLeft :: GameState -> GameState
emptyLeft (G l r) = (G (empty l) r)

emptyRight :: GameState -> GameState
emptyRight (G l r) = (G l (empty r))

transitions :: [GameState -> GameState]
transitions =
  [ transferLeftToRight,
    transferRightToLeft,
    fillLeft,
    fillRight,
    emptyLeft,
    emptyRight
  ]

nextStates :: GameState -> [GameState]
nextStates gs = filter (/=gs) $ map ($ gs) transitions
