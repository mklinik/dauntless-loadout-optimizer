module Main where

import Data.List
import qualified Data.Map as Map
import Data.Map (Map,(!))

import Histogram
import Types
import Database

allLoadouts =
  [ (l, loadoutResistances l, loadoutAdvantage l, loadoutSlots l, loadoutPerks l)
  | weapon <-
      -- [h | h@(Equipment name _ _ _ _) <- chainBlades, "Skarn" `isPrefixOf` name]
      [h | h@(Equipment name _ _ _ _) <- hammers, "Boreus" `isInfixOf` name]
      -- hammers
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
      -- sortOn (\(_,_,advantage,_,_) ->   (num advantage Blaze)) $
      sortOn (\(_,rests,_,_,_) ->     - (num rests Blaze))     $

      -- sortOn (\(_,_,_,_,perks) -> - (num perks Fortress)) $
      -- sortOn (\(_,_,_,_,perks) -> - (num perks Conditioning)) $

      -- optimize for neutral
      -- sortOn (\(_,rests,_,_,_) ->     (absSum rests))     $
      -- sortOn (\(_,_,advantage,_,_) -> (absSum advantage)) $

      [ x
      | x@(loadout, resist, advantage, slots, perks) <- allLoadouts
      -- , num slots Mobility >= 2
      -- , num slots Technique >= 1
      -- , num slots Defensive >= 2
      -- , num slots Utility >= 2
      -- , absSum advantage == 0
      -- , num perks Warmth >= 1
      -- , num perks KnockoutKing >= 1
      -- , num perks WeightedStrikes >= 1
      , num perks Conditioning >= 1
      , num perks Evasion >= 1
      , num perks Fireproof >= 2
      -- , num perks Fortress >= 1
      -- , num perks Tough >= 1
      -- , num perks Guardian >= 1
      -- , _name (_weapon loadout) /= "Destiny of Boreus"
      -- , _name (_weapon loadout) /= "Skarn's Vengeance"
      -- , num resist Frost >= 3
      , num resist Blaze >= 4
      ]

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
