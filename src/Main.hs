module Main where

import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.IO(hGetContents, stdin, openFile, IOMode(ReadMode))
import Text.ParserCombinators.Parsec(Parser, parse, (<|>), many, noneOf, try, between, char, string, sepBy1)

type CSV = [Record]
type Record = [Field]
type Field = String

record :: Char -> Parser Record
record d = (quotedField <|> field d) `sepBy1` char d

field :: Char -> Parser Field
field d = many (noneOf $ d:"\n\r\"")

quotedField :: Parser Field
quotedField = between (char '"') (char '"') $
              many (noneOf "\"" <|> try (string "\"\"" >> return '"'))

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (==x) xs

parseCSV :: String -> IO (Maybe CSV)
parseCSV filename = case filename of
                      "-" -> parseCSV' "(stdin)" stdin
                      _   -> parseCSV' filename =<< openFile filename ReadMode
  where
    parseCSV' filename' handle = do
      contents <- hGetContents handle
      let delim = if count ';' contents > count ',' contents then ';' else ','
      return $ mapM (either (const Nothing) Just . parse (record delim) filename') $ lines contents

padRight :: Int -> String -> String
padRight n s = s ++ replicate (max 0 (n - length s)) ' '

padRight' :: [Int] -> [Int] -> [Int]
padRight' a b = zipWith const (a ++ [0, 0..]) (if length a > length b then a else b)

getLengths :: [[String]] -> [Int]
getLengths csv = getLengths' [] $ map (map length) csv
  where
    getLengths' r []     = map (\a -> min 100 (a+1)) r
    getLengths' r (x:xs) = getLengths' (zipWith max (padRight' r x) (padRight' x r)) xs

showCSV :: CSV -> String
showCSV csv = showCSV' (init (getLengths csv) ++ [0]) csv
  where
    showCSV' :: [Int] -> [[String]] -> String
    showCSV' _ []           = ""
    showCSV' lengths (x:xs) = concat (zipWith padRight lengths x) ++ ('\n' : showCSV' lengths xs)

main :: IO ()
main = do
  args <- getArgs
  csvs <- case args of
    [] -> (:[]) <$> parseCSV "-"
    _  -> mapM parseCSV args
  maybe exitFailure (putStr . showCSV . concat) (sequence csvs)
