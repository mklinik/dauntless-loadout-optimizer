module Main where

import Data.List
import qualified Data.Map as Map
import Data.Map (Map,(!))

import Histogram
import Types
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
  [ (l, loadoutResistances l, loadoutAdvantage l, loadoutSlots l, loadoutPerks l)
  | weapon <- hammers
  , helm <- helms
  , bodyArmor <- bodyArmors
  , gauntlet <- gauntlets
  , boots <- bootss
  , lantern <- lanterns
  , let l = Loadout weapon helm bodyArmor gauntlet boots lantern
  ]

main :: IO ()
main = do

  let
    loadoutRequirements =
      -- optimize for blaze
      -- sortOn (\(_,_,advantage,_) ->   (num advantage Blaze)) $
      -- sortOn (\(_,rests,_,_) ->     - (num rests Blaze))     $

      -- optimize for frost
      sortOn (\(_,_,advantage,_,_) ->   (num advantage Frost)) $
      sortOn (\(_,rests,_,_,_) ->     - (num rests Frost))     $

      -- optimize for neutral
      -- sortOn (\(_,_,advantage,_) -> - (absSum advantage)) $
      -- sortOn (\(_,rests,_,_) ->       (absSum rests))     $

      -- sortOn (\(_,_,_,_,perks) -> - (num perks KnockoutKing)) $
      sortOn (\(_,_,_,_,perks) -> - (num perks Warmth)) $

      [ x
      | x@(_, resist, _, slots, perks) <- allLoadouts
      , num slots Mobility >= 1
      , num slots Power >= 2
      , num slots Technique >= 2
      , num slots Defensive >= 1
      -- , num perks Warmth >= 1
      , num perks KnockoutKing >= 1
      , num resist Frost >= 3
      ]

  print $ length loadoutRequirements
  mapM_ (\(l,rests,_,slots,perks) -> do
      print l
      print rests
      print slots
      print perks
      putStrLn "") $
    take 3 $
    loadoutRequirements
