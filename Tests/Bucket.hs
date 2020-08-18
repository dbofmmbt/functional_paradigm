module Tests.Bucket where

import Src.Bucket
import Tests.Index

a = Bucket {capacity = 3, current = 0}

b = Bucket {capacity = 5, current = 0}

transferTests =
  [ Test "Both buckets empty" $ transfer a b == (a, b),
    Test "Both buckets full" $ transfer (a {current = 3}) (b {current = 5}) == (a {current = 3}, b {current = 5}),
    Test "A full and B empty" $ transfer (a {current = 3}) b == (a, b {current = 3}),
    Test "A empty and B full" $ transfer a (b {current = 5}) == (a, b {current = 5})
  ]
