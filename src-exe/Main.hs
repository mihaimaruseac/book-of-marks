import BookmarkManager.Types (inc)

main :: IO ()
main = print . inc $ (41 :: Int)
