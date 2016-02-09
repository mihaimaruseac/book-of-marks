import Criterion
import Criterion.Main

import BookmarkManager.Types (inc)

main :: IO ()
main = defaultMain [bench "inc 41" (whnf inc (41 :: Int))]
