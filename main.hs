import Data.List (sort, intersperse, intercalate)
import Data.Maybe (mapMaybe)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Read (readMaybe)

main :: IO ()
main = do
  contents <- readFromFile "prices.txt"
  let lns = lines contents
  let nums = mapMaybe readMaybe lns :: [Double]
  putStrLn ("Data: " ++ (show . sort) nums)
  putStrLn ("Average: " ++ (show . average) nums)
  putStrLn ("Median: " ++ (show . median) nums)

readFromFile :: FilePath -> IO String
readFromFile fileName = do
  file <- openFile fileName ReadMode
  hGetContents file

average :: (Real a, Fractional b) => [a] -> b
average x = realToFrac (sum x) / fromIntegral (length x)

middle :: [a] -> [a]
middle [] = []
middle t = m t t
  where
    m (x : _) [_] = [x] -- Reached end, odd length
    m (x : y : _) [_, _] = [x, y] -- Reached end, even length
    m (_ : u) (_ : _ : v) = m u v -- Move iterators forwards by 1 and 2 respectively
    m _ _ = [] -- Match all remaining patterns (never occur) to make compiler happy

median :: (Real a, Fractional b) => [a] -> b
median = average . middle . sort