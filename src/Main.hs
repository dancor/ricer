module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import FUtil
import System.Console.GetOpt
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import System.Process
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

-- up to 60
setLevel :: FilePath -> Int -> IO ()
setLevel home l = 
  myRun "wget" ["--save-cookies", home </> ".ricer" </> "cookies",
    "http://freerice.com/set_level/" ++ show l] >> return ()

myRun :: FilePath -> [String] -> IO String
myRun c as = do
  -- todo: error checking?
  (_ec, out, _err) <- readProcessWithExitCode c as ""
  return out

frPage = "http://freerice.com/game.php"

getPage :: FilePath -> String -> Maybe Params -> IO String
getPage home page postDataMb = myRun "wget" args where
  args = 
     ["--load-cookies", home </> ".ricer" </> "cookies", "-O", "-", page] ++
     postDataArgs
  postDataArgs = case postDataMb of
    Nothing -> []
    Just params -> ["--post-data",
      intercalate "&" . map (\ (k, v) -> k ++ "=" ++ v) $ M.toList params]

tagsToParams :: [Tag String] -> Params
tagsToParams = 
  M.fromList . map toPair . filter (tagOpenLit "input" (const True))
  where
  toPair (TagOpen _ a) = 
    (fromJust $ lookup "name" a, fromJust $ lookup "value" a)

tagsToCorrectStr :: [Tag String] -> Maybe String
tagsToCorrectStr tags = case incorrectsToEnd of
  [] -> Nothing
  TagText t:_ ->
    Just . reverse . dropWhile isSpace . reverse $ dropWhile isSpace t
  _ -> error "unexpected response from freerice.."
  where 
  incorrectsToEnd = drop 1 $ dropWhile
    (not . tagOpenAttrNameLit "div" "id" (== "incorrect")) tags

whileM t f = t >>= \ r -> when r (f >> whileM t f)

getUserAnswer :: Opt.Opts -> String -> IO (Maybe Int)
getUserAnswer opts s = do
  whileM (hReady stdin) getChar
  putStr $ "\n" ++ s
  let
    doingItWrong = do
      putStrLn "Enter one of the word choices."
      getUserAnswer opts s
  c <- getChar
  if c == 'q' then return Nothing else
    case (+ 1) <$> elemIndex c (Opt.numbering opts) of
      Nothing -> doingItWrong
      Just i -> if i < 1 || i > 4 then doingItWrong else return $ Just i

tagTextMb :: Tag t -> Maybe t
tagTextMb (TagText t) = Just t
tagTextMb _ = Nothing

playGame :: Opt.Opts -> FilePath -> Maybe Params -> State -> IO ()
playGame opts home paramsMb (State roundNum rightNum lastWord lastGuess) = do
  tags <- parseTags <$> getPage home frPage paramsMb
  home <- getEnv "HOME"
  (correctStr, rightNum') <- case paramsMb of
    Nothing -> return ("", rightNum)
    _ -> case tagsToCorrectStr tags of
      Nothing -> return ("correct, " ++ lastWord ++ " = " ++ lastGuess,
        rightNum + 1)
      Just s -> do
        appendFile (home ++ "/.ricer/wrong") (words s !! 1 ++ "\n")
        putStrLn ""
        let
          wrongPre = "Incorrect! "
          typeLine = drop (length wrongPre) s
          repent = do
            putStrLn $ wrongPre ++ "Please type: " ++ typeLine
            penatence <- getLine
            when (penatence /= typeLine) $ repent
        repent
        return ("", rightNum)
  let
    word = head . catMaybes . map tagTextMb $
      dropWhile (/= TagOpen "div" [("id","question-title")]) tags
    params = tagsToParams tags
    wdList = breaks (== '|') . fromJust $ M.lookup "list" params
    topStr = show rightNum' ++ " / " ++ show roundNum ++ ": " ++ correctStr
    keepAsking = case Opt.tillNRight opts of
      Nothing -> True
      Just n -> n > rightNum'
  clrScr
  putStrLn topStr
  when keepAsking $ do
    ans <- getUserAnswer opts . unlines $
      word:"":zipWith (\ n c -> [n] ++ " " ++ c) (Opt.numbering opts) wdList
    case ans of
      Nothing -> return ()
      Just i ->
        playGame opts home
          (Just . M.insert "op" "next" $ 
            M.insert "answer" (show $ i - 1) params) $
          State (roundNum + 1) rightNum' word $ wdList !! (i - 1)

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
  setLevel home $ Opt.level opts
  playGame opts home Nothing startState
