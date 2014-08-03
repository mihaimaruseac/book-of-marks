import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Data.List (sort)

main :: IO ()
main = defaultMain $ testGroup "library-tests" tests

tests :: [TestTree]
tests =
  [ testGroup "SmallCheck" smallCheckTests
  , testGroup "Unit tests" unitTests
  ]

smallCheckTests :: [TestTree]
smallCheckTests =
  [ testProperty "sort == sort . reverse" $ prop_sort
  , testProperty "Fermat's last theorem" $ prop_fermat
  ]

unitTests :: [TestTree]
unitTests =
  [ testCase "Lists of different length" $ case_diff_len
  , testCase "Lists of same length" $ case_same_len
  ]

prop_sort :: [Int] -> Bool
prop_sort l = sort l == sort (reverse l)

prop_fermat :: Monad m => Integer -> Integer -> Integer -> Integer -> Property m
prop_fermat x y z n = and [x > 0, y > 0, z > 0, n >= 3] ==> x^n + y^n /= z^n

case_diff_len, case_same_len :: Assertion
case_diff_len = [1 :: Int, 2, 3] `compare` [1, 2] @?= GT
case_same_len = [1 :: Int, 2, 3] `compare` [1, 2, 2] @?= GT
