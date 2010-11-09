{-# LANGUAGE TemplateHaskell #-}

module Opt where

import Data.PolyOpt

$(polyOpt [
  reqArgGen ["till-n-right"] "n"
    "N" [t|Maybe Int|] [|Nothing|] [|Just . read|]
    "exit after getting N right",
  reqArgGen ["numbering"] "N"
    "STR" [t|String|] [|"1234"|] [|id|]
    "use a custom set of characters to number the four\noptions",
  reqArgGen ["level"] "l"
    "N" [t|Int|] [|36|] [|read|]
    "set level (1..60, default 36)"
  ])
