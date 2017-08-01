{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE MultiParamTypeClasses #-} 

module CustomStack(CustomStack(..)) where
  import Prelude hiding (head, tail)
  import Stack

  data CustomStack a = Nil | Cons a (CustomStack a) deriving Show

  instance Stack CustomStack a where
    empty = Nil
    isEmpty Nil = True
    isEmpty _ = False
    cons b a = Cons b a
    head Nil = error "CustomStack Empty"
    head (Cons a _) = a
    tail Nil = error "CustomStack Empty"
    tail (Cons _ b) = b

