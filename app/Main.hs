module Main where

import Data.List (intercalate, intersperse, sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text, append, pack)
import Data.Text.Format.Numbers (PrettyCfg (PrettyCfg), prettyF)
import Data.Text.IO (putStrLn)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Text.Read (readMaybe)

formatPrice :: RealFrac a => a -> Text
formatPrice = prettyF (PrettyCfg 2 (Just ',') '.')

main :: IO ()
main = do
  contents <- readFromFile "prices.txt"
  let lns = lines contents
  let nums = map round (mapMaybe readMaybe lns :: [Double])
  Data.Text.IO.putStrLn (pack "Average: $" `append` (formatPrice . average) nums)
  Data.Text.IO.putStrLn (pack "Median: $" `append` (formatPrice . median) nums)

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