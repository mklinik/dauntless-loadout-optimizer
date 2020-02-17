module Main where

import Data.List

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
  [ (l, loadoutResistances l, loadoutSlots l)
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
  mapM_ print $ take 3 $ sortOn (\(_,rests,_) -> - (_shock rests))
    [ x
    | x@(l, rests, slots) <- allLoadouts
    , _mobility slots >= 1
    , _power slots >= 1
    , _technique slots >= 2
    , _defensive slots >= 1
    ]
