module Src.Bucket where

data Bucket = Bucket {capacity :: Int, current :: Int} deriving (Eq, Show)

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
