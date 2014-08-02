import System.FilePath.Glob
import Test.DocTest

main = glob "src/**/*.hs" >>= doctest
