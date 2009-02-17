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
import System.IO
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import qualified Data.Map as M

data Options = Options {
  optTillNRight :: Maybe Int,
  optNumbering :: String
}

defOpts :: Options
defOpts = Options {
  optTillNRight = Nothing,
  optNumbering = "1234"
}

options :: [OptDescr (Options -> Options)]
options = [
  Option "n" ["till-n-right"]
    (ReqArg (\ a o -> o {optTillNRight = Just $ read a}) "N")
    "exit after getting N right",
  Option "N" ["numbering"]
    (ReqArg (\ a o -> o {optNumbering = a}) "STR")
    "use a custom set of characters to number the four options"
  ]

type Params = M.Map String String

data State = State {
  roundNum :: Int,
  rightNum :: Int,
  lastWord :: String,
  lastGuess :: String
  }

startState :: State
startState = State 0 0 "" ""

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

collectAnswer :: Options -> String -> IO (Maybe Int)
collectAnswer opts s = do
  putStr $ "\n" ++ s
  let
    doingItWrong = do
      putStrLn "Enter one of the word choices."
      collectAnswer opts s
  c <- getChar
  if c == 'q' then return Nothing else
    case (+ 1) <$> elemIndex c (optNumbering opts) of
      Nothing -> doingItWrong
      Just i -> if i < 1 || i > 4 then doingItWrong else return $ Just i

playGame :: Options -> Maybe Params -> State -> IO ()
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
    keepAsking = case optTillNRight opts of
      Nothing -> True
      Just n -> n > rightNum'
  clrScr
  putStrLn topStr
  when keepAsking $ do
    ans <- collectAnswer opts . unlines $
      word:"":zipWith (\ n c -> [n] ++ " " ++ c) (optNumbering opts) choices
    case ans of
      Nothing -> return ()
      Just i ->
        playGame opts (Just params') .
          State (roundNum + 1) rightNum' word $ choices !! (i - 1)
        where params' = M.insert "SELECTED" (show i) params

main :: IO ()
main = do
  (opts, args) <- doArgs "usage" defOpts options
  let [] = args
  hSetBuffering stdin NoBuffering
  home <- getEnv "HOME"
  createDirectoryIfMissing False $ home ++ "/.ricer"
  playGame opts Nothing startState
