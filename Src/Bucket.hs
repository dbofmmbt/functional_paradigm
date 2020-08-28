module Src.Bucket where

import Data.List
import Data.List

type Volume = Int
data Bucket = Bucket {capacity :: Volume, current :: Volume} deriving (Eq)

instance Show Bucket where
  show = show . current

new :: Volume -> Bucket
new v = Bucket v 0

fill :: Bucket -> Bucket
fill b = b {current = capacity b}

empty :: Bucket -> Bucket
empty b = b {current = 0}

freeSpace :: Bucket -> Int
freeSpace b = capacity b - current b

transfer :: Bucket -> Bucket -> (Bucket, Bucket)
transfer a b
  | totalTransfer = (a {current = 0}, b {current = current b + current a})
  | otherwise = (a {current = current a - freeSpace b}, b {current = capacity b})
  where
    totalTransfer = freeSpace b >= current a
