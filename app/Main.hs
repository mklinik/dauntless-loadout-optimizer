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
    sortOn (\(_,_,advantage,_) -> (_blaze advantage)) $
    sortOn (\(_,rests,_,_) -> - (_blaze rests)) $
    [ x
    | x@(_, _, _, slots) <- allLoadouts
    , _mobility slots >= 1
    , _power slots >= 2
    , _technique slots >= 1
    , _defensive slots >= 1
    ]
