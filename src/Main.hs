module Main where

import Control.Applicative
import Data.Char
import Data.List
import FUtil
import HSH
import System.Directory
import System.Environment
import System.IO
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Data.Map as M

type Params = M.Map String String

data State = State {
  roundNum :: Int,
  rightNum :: Int
  }

startState :: State
startState = State 0 0

getTags :: Maybe Params -> IO [Tag]
getTags postDataMb = let 
  postDataArgs = case postDataMb of
    Nothing -> []
    Just params -> ["--post-data", 
      intercalate "&" . map (\ (k, v) -> k ++ "=" ++ v) $ M.toList params]
  args = ["-O", "-", "http://freerice.com"] ++ postDataArgs
  in parseTags <$> run ("hide_errs", "wget":args)

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

collectAnswer :: String -> String -> IO (Maybe Int)
collectAnswer msg s = do
  clrScr
  putStr $ msg ++ "\n\n" ++ s
  let
    doingItWrong = collectAnswer "Enter one of the number choices." s
  c <- getChar
  if c == 'q' then return Nothing else case readMb [c] of
    Nothing -> doingItWrong
    Just i -> if i < 1 || i > 4 then doingItWrong else return $ Just i

playGame :: Maybe Params -> State -> IO ()
playGame paramsMb (State roundNum rightNum) = do
  tags <- getTags paramsMb
  home <- getEnv "HOME"
  (correctStr, rightNum') <- case paramsMb of
    Nothing -> return ("", rightNum)
    _ -> case tagsToCorrectStr tags of
      Nothing -> return ("correct!", rightNum + 1)
      Just s -> do
        appendFile (home ++ "/.ricer/wrong") (head (words s) ++ "\n")
        return ("wrong!  " ++ s, rightNum)
  let
    params = tagsToParams tags
    -- lol param name..
    Just wordStr = M.lookup "INFO3" params
    word:choices = breaks (== '|') wordStr
    topStr = show rightNum' ++ " / " ++ show roundNum ++ ": " ++ correctStr
  ans <- collectAnswer topStr . unlines $ 
    word:"":zipWith (\ n c -> show n ++ " " ++ c) [1..] choices
  case ans of
    Nothing -> return ()
    Just i -> let params' = M.insert "SELECTED" (show i) params
      in playGame (Just params') $ State (roundNum + 1) rightNum'

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  home <- getEnv "HOME"
  createDirectoryIfMissing False $ home ++ "/.ricer"
  playGame Nothing startState
