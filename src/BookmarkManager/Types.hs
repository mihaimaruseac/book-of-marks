module BookmarkManager.Types where

{-|
Small test function to test the doctest support.

   >>> let answer = 42 :: Int
   >>> let prev = answer - 1
   >>> testDocTest prev
   42
   >>> succ . last . take prev . iterate testDocTest $ 1
   42
 -}
testDocTest :: Num a => a -> a
testDocTest x = x + 1
