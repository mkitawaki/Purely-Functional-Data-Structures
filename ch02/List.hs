module List(List(..)) where
  import Prelude hiding (head, tail)
  import Stack

  data List a = L [a] deriving Show

  instance Stack List where
    empty = L []
    isEmpty (L a) = null a
    cons b (L a) = L (a ++ [b]) 
    head (L (x:xs)) = x
    tail (L (x:xs)) = L xs
