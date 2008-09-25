module Main where

import Control.Applicative
import Data.Char
import Data.List
import FUtil
import HSH
import System.IO
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Data.Map as M

type Params = M.Map String String

getTags :: Maybe Params -> IO [Tag]
getTags postDataMb = let 
  postDataArgs = case postDataMb of
    Nothing -> []
    Just params -> ["--post-data", 
      intercalate "&" . map (\ (k, v) -> k ++ "=" ++ v) $ M.toList params]
  args = ["-O", "-", "http://freerice.com"] ++ postDataArgs
  in parseTags <$> run ("wget", args)

tagsToParams :: [Tag] -> Params
tagsToParams = let
  -- I should have used Maybe but List's were briefer; am I going to hell?
  toPair (TagOpen _ [_, ("name", n), ("value", v)]) = [(n, v)]
  toPair _ = []
  in M.fromList . concatMap toPair . filter (isTagOpenName "input")

tagsToCorrectStr :: [Tag] -> Maybe [Char]
tagsToCorrectStr tags = let
  incorrectsToEnd = filter 
    (tagOpenAttrNameLit "div" "id" (== "incorrect") . head) . 
    init $ tails tags 
  in case incorrectsToEnd of
    [] -> Nothing
    (_:TagText t:_):_ -> 
      Just . reverse . dropWhile isSpace . reverse $ dropWhile isSpace t
    _ -> error "unexpected response from freerice.."

collectAnswer :: String -> String -> IO Int
collectAnswer msg s = do
  clrScr
  putStr $ msg ++ "\n\n" ++ s
  let
    doingItWrong = collectAnswer "Enter one of the number choices." s
  c <- getChar
  case readMb [c] of
    Nothing -> doingItWrong
    Just i -> if i < 1 || i > 4 then doingItWrong else return i

playGame :: Maybe Params -> IO ()
playGame paramsMb = do
  tags <- getTags paramsMb
  let
    params = tagsToParams tags
    correctStr = case paramsMb of
      Nothing -> ""
      _ -> case tagsToCorrectStr tags of
        Nothing -> "correct!"
        Just s -> "wrong!  " ++ s
    -- lol param name..
    Just wordStr = M.lookup "INFO3" params
    word:choices = breaks (== '|') wordStr
  c <- collectAnswer correctStr . unlines $ 
    word:"":zipWith (\ n c -> show n ++ " " ++ c) [1..] choices
  let params' = M.insert "SELECTED" (show c) params
  playGame $ Just params'

main :: IO ()
main = hSetBuffering stdin NoBuffering >> playGame Nothing
