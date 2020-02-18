module Main where

import Data.List
import qualified Data.Map as Map
import Data.Map (Map,(!))

import Database

testLoadout = Loadout
  { _weapon = weapons !! 0
  , _helm = helms !! 0
  , _bodyArmor = bodyArmors !! 0
  , _gauntlet = gauntlets !! 0
  , _boots = bootss !! 0
  , _lantern = lanterns !! 0
  }

allLoadouts =
  [ (l, loadoutResistances l, loadoutAdvantage l, loadoutSlots l)
  | weapon <- weapons
  , helm <- helms
  , bodyArmor <- bodyArmors
  , gauntlet <- gauntlets
  , boots <- bootss
  , lantern <- lanterns
  , let l = Loadout weapon helm bodyArmor gauntlet boots lantern
  ]

main :: IO ()
main = do
  mapM_ (\(l,rests,_,slots) ->
      print l >>
      print rests >>
      print slots >>
      putStrLn "") $
    take 3 $
    sortOn (\(_,_,advantage,_) -> - (num advantage Blaze)) $
    sortOn (\(_,rests,_,_) ->     - (num rests Blaze))     $
    [ x
    | x@(_, _, _, slots) <- allLoadouts
    , num slots Mobility >= 1
    , num slots Power >= 2
    , num slots Technique >= 1
    , num slots Defensive >= 1
    ]
