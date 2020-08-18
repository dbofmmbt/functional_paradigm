module Src.Functor where

mymap :: (a -> b) -> ([a] -> [b])
mymap f [] = []
mymap f (x : xs) = (f x) : (mymap f xs)

data F s a = F (s -> a)

mapF :: (a -> b) -> F s a -> F s b
mapF f (F g) = F (f . g)

runF :: F s a -> (s -> a)
runF (F g) = g
