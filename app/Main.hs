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
  | weapon <- chainBlades
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
      -- optimize for frost
      -- sortOn (\(_,_,advantage,_,_) ->   (num advantage Shock)) $
      -- sortOn (\(_,rests,_,_,_) ->     - (num rests Shock))     $

      -- sortOn (\(_,_,_,_,perks) -> - (num perks Fortress)) $
      sortOn (\(_,_,_,_,perks) -> - (num perks Conditioning)) $

      -- optimize for neutral
      sortOn (\(_,rests,_,_,_) ->     (absSum rests))     $
      sortOn (\(_,_,advantage,_,_) -> (absSum advantage)) $

      test

  -- print $ length allLoadouts
  print $ length loadoutRequirements
  mapM_ (\(loadout,rests,advantage,slots,perks) -> do
      print loadout
      print rests
      print slots
      print perks
      putStrLn "") $
    -- take 4 $
    loadoutRequirements

test =
  [ x
  | x@(loadout, resist, advantage, slots, perks) <- allLoadouts
  , num slots Mobility >= 2
  , num slots Technique >= 2
  , num slots Defensive >= 2
  -- , absSum advantage == 0
  -- , num slots Defensive >= 1
  -- , num perks Warmth >= 1
  -- , num perks KnockoutKing >= 1
  -- , num perks WeightedStrikes >= 1
  , num perks Conditioning >= 1
  , num perks Fortress >= 1
  , num perks Guardian >= 1
  -- , _name (_weapon loadout) /= "Destiny of Boreus"
  -- , _name (_weapon loadout) /= "Skarn's Vengeance"
  -- , num resist Frost >= 3
  ]
