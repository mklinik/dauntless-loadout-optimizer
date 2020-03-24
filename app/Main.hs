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
      [h | h@(Equipment name _ _ _ _) <- chainBlades, "Thundering Cutters" `isInfixOf` name]
      -- [h | h@(Equipment name _ _ _ _) <- hammers, "Boreus" `isInfixOf` name]
      -- hammers
      -- chainBlades
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
    foundLoadouts =
      -- sortOn (\(_,_,advantage,_,_) ->   (num advantage Blaze)) $
      -- sortOn (\(_,rests,_,_,_) ->     - (num rests Blaze))     $

      -- sortOn (\(_,_,_,_,perks,_) -> - (num Fireproof perks)) $
      -- sortOn (\(_,_,_,_,perks) -> - (num perks Conditioning)) $

      -- optimize for neutral
      -- sortOn (\(_,rests,_,_,_) ->     (absSum rests))     $
      -- sortOn (\(_,_,advantage,_,_) -> (absSum advantage)) $
      sortOn (\(_,_,advantage,slots,_,_) -> - (num Technique slots)) $
      sortOn (\(_,_,advantage,_,_,Just(_,remainingPerks)) -> absSum remainingPerks) $

      [ (loadout, resist, advantage, slots, perks, remaining)
      | (loadout, resist, advantage, slots, perks) <- allLoadouts
      , let remaining =
              Just (slots, perks)
                >>= has Defensive Fortress
                >>= has Defensive Fortress
                >>= has Defensive Bloodless
                >>= has Mobility Conditioning
                >>= has Mobility Agility
                >>= has Utility Medic
                >>= has Utility Medic
                -- >>= has Power Sharpened
                -- >>= exclude   Rage
      -- , absSum advantage == 0
      -- , num resist Frost >= 3
      , maybe False (const True) remaining
      , num Terra resist >= 3
      -- , num Blaze advantage >= 1
      ]

  -- print $ length allLoadouts
  print $ length foundLoadouts
  mapM_ (\(loadout,rests,advantage,slots,perks,Just (remainingSlots,remainingPerks)) -> do
      print loadout
      print rests
      putStrLn "has:"
      print slots
      print perks
      putStrLn "after required:"
      print remainingSlots
      print remainingPerks
      putStrLn "") $
    -- take 4 $
    foundLoadouts

has :: Slot -> Perk -> (Slots, Perks) -> Maybe (Slots, Perks)
has slot perk (slots, perks)
  | num perk perks > 0 = Just (slots, dec perk perks)
  | num slot slots > 0 = Just (dec slot slots, perks)
  | otherwise = Nothing

-- hasPerk :: Slot -> Perk -> (Slots, Perks) -> Maybe (Slots, Perks)
-- hasPerk perk (slots, perks)
  -- | num perk perks > 0 = Just (slots, dec perk perks)
  -- | otherwise = Nothing

exclude :: Perk -> (Slots, Perks) -> Maybe (Slots, Perks)
exclude perk x@(_, perks)
  | num perk perks > 0 = Nothing
  | otherwise = Just x
