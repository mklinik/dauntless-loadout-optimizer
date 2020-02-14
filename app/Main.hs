module Main where

import Data.List

import Database


myLoadout = Loadout
  { weapon = weapons !! 0
  , helm = helms !! 0
  , bodyArmor = bodyArmors !! 0
  , gauntlet = gauntlets !! 0
  , boots = bootss !! 0
  , lantern = lanterns !! 0
  }

main :: IO ()
main = do
  print $ loadoutSlots myLoadout
