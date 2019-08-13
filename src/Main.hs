module Main where

import System.Environment(getArgs)
import System.Exit(exitFailure)
import System.IO(hGetContents, stdin, openFile, IOMode(ReadMode))
import System.Console.ANSI(setSGRCode, SGR(..), ConsoleLayer(Foreground), ColorIntensity(..), Color(..)) -- from 0.9 of ansi-terminal package: hSupportsANSIColor, setPaletteColor
import Text.ParserCombinators.Parsec(Parser, parse, (<|>), many, many1, noneOf, try, between, char, string, sepBy1, eof, hexDigit, count, digit, optional, oneOf, spaces)
import Data.Either(isRight)
import Data.List(intercalate)

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

count' :: Eq a => a -> [a] -> Int
count' x xs = length $ filter (==x) xs

parseCSV :: String -> IO (Maybe CSV)
parseCSV filename = case filename of
                      "-" -> parseCSV' "(stdin)" stdin
                      _   -> parseCSV' filename =<< openFile filename ReadMode
  where
    parseCSV' filename' handle = do
      contents <- hGetContents handle
      let delim = if count' ';' contents > count' ',' contents then ';' else ','
      return $ mapM (either (const Nothing) Just . parse (record delim) filename') $ lines contents

parse' :: Parser String -> String -> Bool
parse' p = isRight . parse (p >> eof) ""

looksLikeHex :: String -> Bool
looksLikeHex = parse' $ many hexDigit

looksLikeHash :: String -> Bool
looksLikeHash = parse' $ (try (char '0' >> char 'x' >> count 2 hexDigit)
                          <|> (char '#' >> count 4 hexDigit)
                          <|> count 6 hexDigit
                         ) >> many hexDigit

number :: Parser String
number = optional (oneOf "-+") >> many1 digit >> (many (char ',' >> count 3 digit) <|> many (char '\'' >> count 3 digit)) >> optional (char '.' >> many1 digit) >> return ""

looksLikeNumber :: String -> Bool
looksLikeNumber = parse' number

looksLikeMoney :: String -> Bool
looksLikeMoney = parse' $ (count 1 (oneOf "$£€") <|> string "CHF" <|> string "AUD" <|> string "AU$") >> spaces >> number

colorify :: Bool -> Color -> String -> String
colorify withColour col x = if withColour then setSGRCode [SetColor Foreground Dull col] ++ x ++ setSGRCode [Reset] else x

colorify' :: Bool -> Color -> String -> String
colorify' withColour col x = if withColour then setSGRCode [SetColor Foreground Dull col, SetItalicized True] ++ x ++ setSGRCode [Reset] else x

(||.) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(||.) p q x = p x || q x

pad :: Bool -> Int -> String -> String
pad withColour n s
  | looksLikeHash s = colorify' withColour Red s ++ spaces'
  | looksLikeNum s  = spaces' ++ colorify withColour Blue s
  | otherwise       = s ++ spaces'
  where
    spaces' = replicate (n - length s - 1) ' '
    looksLikeNum = looksLikeNumber ||. looksLikeMoney

calcPadding :: [Int] -> [Int] -> [Int]
calcPadding a b = zipWith const (a ++ [0, 0..]) (if length a > length b then a else b)

getLengths :: [[String]] -> [Int]
getLengths csv = getLengths' [] $ map (map length) csv
  where
    getLengths' r []     = map (\a -> min 50 (a+1)) r
    getLengths' r (x:xs) = getLengths' (zipWith max (calcPadding r x) (calcPadding x r)) xs

showCSV :: Bool -> CSV -> String
showCSV withColour csv = showCSV' (getLengths csv) csv
  where
    showCSV' :: [Int] -> [[String]] -> String
    showCSV' _ []           = ""
    showCSV' lengths (x:xs) = intercalate sep (zipWith (pad withColour) lengths (x ++ repeat "")) ++ ('\n' : showCSV' lengths xs)
    sep = colorify withColour Yellow "│"

main :: IO ()
main = do
  args <- getArgs
  csvs <- case args of
    [] -> (:[]) <$> parseCSV "-"
    _  -> mapM parseCSV args
  maybe exitFailure (putStr . showCSV True . concat) (sequence csvs)
