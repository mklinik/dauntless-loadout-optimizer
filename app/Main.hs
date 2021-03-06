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
      -- [h | h@(Equipment name _ _ _ _) <- chainBlades, "Thundering Cutters" `isInfixOf` name]
      -- [h | h@(Equipment name _ _ _ _) <- hammers, "Boreus" `isInfixOf` name]
      -- [w | w@(Equipment name _ _ _ _) <- pikes, "Godhand" `isInfixOf` name]
      -- [w | w@(Equipment name _ _ _ _) <- strikers, "Legendary" `isInfixOf` name]
      -- [w | w@(Equipment name _ _ _ _) <- strikers, "Boreus" `isInfixOf` name]
      -- hammers
      -- chainBlades
      -- pikes
      strikers
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
      sortOn (\(_,resist,_,_,_,_) ->     - (num Blaze resist))     $

      -- sortOn (\(_,_,_,_,perks,_) -> - (num Fireproof perks)) $
      -- sortOn (\(_,_,_,_,perks) -> - (num perks Conditioning)) $

      -- optimize for neutral
      -- sortOn (\(_,rests,_,_,_,_) ->     (absSum rests))     $
      -- sortOn (\(_,_,advantage,_,_,_) -> (absSum advantage)) $

      -- sortOn (\(_,_,_,_,_,Just(remainingSlots,_)) -> - (num Technique remainingSlots)) $

      -- sort by less garbage
      sortOn (\(_,_,_,_,_,Just(_,remainingPerks)) -> absSum remainingPerks) $


      [ (loadout, resist, advantage, slots, perks, remaining)
      | (loadout, resist, advantage, slots, perks) <- allLoadouts
      , let remaining =
              Just (slots, perks)
                >>= has Defensive Iceborne
                >>= has Defensive Iceborne
                >>= has Defensive Tough
                >>= has Defensive Tough
                >>= has Power Rage
                >>= has Power Rage
                >>= has Mobility Conditioning
                >>= has Technique Berserker
                >>= has Power Ragehunter
                >>= has Defensive Fireproof
                >>= has Defensive Fireproof
                -- >>= hasSlot Utility
                -- >>= hasSlot Defensive
      , maybe False (const True) remaining
      -- , absSum advantage == 0
      -- , absSum resist <= 4
      -- , num resist Frost >= 3
      -- , maxElem resist <= 1
      -- , num Terra resist >= 3
      -- , num Blaze resist >= 3
      -- , num Frost resist >= -2
      -- , num Terra resist >= -2
      -- , num Blaze resist >= -1
      -- , num Shock resist >= -2
      -- , num Umbral resist >= -2
      -- , num Radiant resist >= -2
      -- , num Blaze advantage >= 1
      , num Frost advantage >= 1
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

-- Require either the given slot or the given perk; favours perk
has :: Slot -> Perk -> (Slots, Perks) -> Maybe (Slots, Perks)
has slot perk (slots, perks)
  | num perk perks > 0 = Just (slots, dec perk perks)
  | num slot slots > 0 = Just (dec slot slots, perks)
  | num Prismatic slots > 0 = Just (dec Prismatic slots, perks)
  | otherwise = Nothing

-- Require the given slot
hasSlot :: Slot -> (Slots, Perks) -> Maybe (Slots, Perks)
hasSlot slot (slots, perks)
  | num slot slots > 0 = Just (dec slot slots, perks)
  | num Prismatic slots > 0 = Just (dec Prismatic slots, perks)
  | otherwise = Nothing

-- hasPerk :: Slot -> Perk -> (Slots, Perks) -> Maybe (Slots, Perks)
-- hasPerk perk (slots, perks)
  -- | num perk perks > 0 = Just (slots, dec perk perks)
  -- | otherwise = Nothing

exclude :: Perk -> (Slots, Perks) -> Maybe (Slots, Perks)
exclude perk x@(_, perks)
  | num perk perks > 0 = Nothing
  | otherwise = Just x
