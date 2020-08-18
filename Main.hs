module Main where

import Tests.Bucket
import Tests.Index

main :: IO ()
main = print $ runTest transferTests