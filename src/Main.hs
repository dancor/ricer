module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import FUtil
import HSH
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Data.Map as M

import qualified Opt

type Params = M.Map String String

data State = State {
  roundNum :: Int,
  rightNum :: Int,
  lastWord :: String,
  lastGuess :: String
  }

startState :: State
startState = State 0 0 "" ""

getTags :: Maybe Params -> IO [Tag String]
getTags postDataMb = let
  postDataArgs = case postDataMb of
    Nothing -> []
    Just params -> ["--post-data",
      intercalate "&" . map (\ (k, v) -> k ++ "=" ++ v) $ M.toList params]
  args = ["-O", "-", "http://freerice.com"] ++ postDataArgs
  in parseTags <$> run ("hide_errs", "wget":args)

tagsToParams :: [Tag String] -> Params
tagsToParams = let
  -- I should have used Maybe but List's were briefer; am I going to hell?
  toPair (TagOpen _ [_, ("name", n), ("value", v)]) = [(n, v)]
  toPair _ = []
  in M.fromList . concatMap toPair . filter (isTagOpenName "input")

tagsToCorrectStr :: [Tag String] -> Maybe String
tagsToCorrectStr tags = let
  incorrectsToEnd = filter
    (tagOpenAttrNameLit "div" "id" (== "incorrect") . head) .
    init $ tails tags
  in case incorrectsToEnd of
    [] -> Nothing
    (_:TagText t:_):_ ->
      Just . reverse . dropWhile isSpace . reverse $ dropWhile isSpace t
    _ -> error "unexpected response from freerice.."

collectAnswer :: Opt.Opts -> String -> IO (Maybe Int)
collectAnswer opts s = do
  putStr $ "\n" ++ s
  let
    doingItWrong = do
      putStrLn "Enter one of the word choices."
      collectAnswer opts s
  c <- getChar
  if c == 'q' then return Nothing else
    case (+ 1) <$> elemIndex c (Opt.numbering opts) of
      Nothing -> doingItWrong
      Just i -> if i < 1 || i > 4 then doingItWrong else return $ Just i

playGame :: Opt.Opts -> Maybe Params -> State -> IO ()
playGame opts paramsMb (State roundNum rightNum lastWord lastGuess) = do
  tags <- getTags paramsMb
  home <- getEnv "HOME"
  (correctStr, rightNum') <- case paramsMb of
    Nothing -> return ("", rightNum)
    _ -> case tagsToCorrectStr tags of
      Nothing -> return ("correct, " ++ lastWord ++ " = " ++ lastGuess,
        rightNum + 1)
      Just s -> do
        appendFile (home ++ "/.ricer/wrong") (head (words s) ++ "\n")
        return ("*********WRONG!********* " ++ s, rightNum)
  let
    params = tagsToParams tags
    -- lol param name..
    Just wordStr = M.lookup "INFO3" params
    word:choices = breaks (== '|') wordStr
    topStr = show rightNum' ++ " / " ++ show roundNum ++ ": " ++ correctStr
    keepAsking = case Opt.tillNRight opts of
      Nothing -> True
      Just n -> n > rightNum'
  clrScr
  putStrLn topStr
  when keepAsking $ do
    ans <- collectAnswer opts . unlines $
      word:"":zipWith (\ n c -> [n] ++ " " ++ c) (Opt.numbering opts) choices
    case ans of
      Nothing -> return ()
      Just i ->
        playGame opts (Just params') .
          State (roundNum + 1) rightNum' word $ choices !! (i - 1)
        where params' = M.insert "SELECTED" (show i) params

usage :: String
usage = "usage: ricer [options]\n" ++ Opt.optInfo

doErrs :: [String] -> a
doErrs errs = error $ concat errs ++ usage

main :: IO ()
main = do
  homeDir <- getHomeDirectory
  (opts, args) <- Opt.getOpts (homeDir </> ".ricer" </> "config") usage
  unless (null args) $ doErrs ["unrecognized args: " ++ show args ++ "\n"]
  hSetBuffering stdin NoBuffering
  home <- getEnv "HOME"
  createDirectoryIfMissing False $ home ++ "/.ricer"
  playGame opts Nothing startState
