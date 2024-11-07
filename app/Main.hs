{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use !!" #-}
module Main where

import System.IO
import Data.List (isPrefixOf)
import Data.Char (isSpace)

newtype Composition = Composition (Char, Char)
  deriving (Show, Eq)

data Tokens
  = Lambda
  | Arrow
  | Dot
  | Colon
  | OpenParen
  | CloseParen
  | CompToken Composition
  | Character Char
  | Types [Char]
  | Int
  | Bool
  | Num
  deriving (Show, Eq)

tokenize :: String -> [Tokens]
tokenize [] = []
tokenize (x : xs)
  | isSpace x = tokenize xs
  | otherwise = case token x xs of
      (tok, rest) -> case tok of
        Character c ->
          if ':' `elem` takeWhile (not . isSpace) xs
            then
              let (typeTok, remaining) = parseTypeAnnotation c xs
               in [Character c, typeTok] ++ tokenize remaining
            else Character c : tokenize rest
        _ -> tok : tokenize rest
  where
    token 'λ' xs'       = (Lambda, xs')
    token ':' xs'       = (Colon, xs')
    token '.' xs'       = (Dot, xs')
    token '(' xs'       = (OpenParen, xs')
    token ')' xs'       = (CloseParen, xs')
    token '-' ('>':xs') = (Arrow, xs')
    token '-' xs'       = (Character '-', xs')
    token c xs'         = (Character c, xs')

    parseTypeAnnotation :: Char -> String -> (Tokens, String)
    parseTypeAnnotation _ xs' =
        let (_, afterColon) = break (== ':') xs'
            (typeName, rest) = break (\x -> isSpace x || x == '.') (drop 1 afterColon)
        in  (if typeName == "Int" 
              then Types "Int"
             else if typeName == "Bool" 
              then Types "Bool"
             else Types typeName, rest)

lexLine :: [String] -> IO [[Tokens]]
lexLine =
  return
    . map tokenize
    . filter (not . isPrefixOf "--" . trim)
    . filter (not . null)
    . map trim
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

parseTokens :: [[Tokens]] -> IO ()
parseTokens [] = return ()
parseTokens (x : xs) = do
  print x
  parseTokens xs

main :: IO ()
main = do
  inFile <- openFile "input.txt" ReadMode
  contents <- hGetContents inFile

  let linesOfContents = lines contents
  tokens <- lexLine linesOfContents
  parseTokens tokens

  hClose inFile
