import Data.Maybe

-- binary tree

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

-- ex2, pg. 60
data MTree a = Mode a (MTree a) (MTree a) 
             | Nothing 
               deriving (Show)

weirdTree = Mode "parent" (Mode "left child" Nothing Nothing)
                          (Mode "right child" Nothing Nothing)
