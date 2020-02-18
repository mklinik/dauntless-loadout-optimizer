{-# LANGUAGE RecordWildCards,NamedFieldPuns #-}
module Database where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List

import Histogram
import Types

weapons = hammers

hammers =
  [ Equipment "Raging Crash"  Weapon Neutral  [Ragehunter]  [Power, Utility]
  , Equipment "Fall of the Shrike"  Weapon Neutral  [Conditioning]  [Power, Mobility]
  , Equipment "Quillshot's Roar"  Weapon Neutral  [Acidic]  [Power, Defensive]
  , Equipment "Skarn's Vengeance"  Weapon Terra [KnockoutKing]  [Power, Defensive]
  , Equipment "Charred Crusher"  Weapon Blaze [Aetherhunter]  [Power, Power]
  , Equipment "Ember Maul"  Weapon Blaze [EvasiveFury]  [Power, Mobility]
  , Equipment "Winter Squall"  Weapon Frost [Nimble]  [Power, Mobility]
  , Equipment "Thundering Maul"  Weapon Shock [AethericAttunement]  [Power, Defensive]
  , Equipment "Nayzaga's Charge"  Weapon Shock [Savagery]  [Power, Utility]
  , Equipment "Pangar's Rampage"  Weapon Frost [KnockoutKing]  [Power, Defensive]
  , Equipment "Inferno's Burden"  Weapon Blaze [Overpower]  [Power, Power]
  , Equipment "Storm Hammer"  Weapon Shock [AethericAttunement]  [Power, Mobility]
  , Equipment "Kharabak's Jaw"  Weapon Terra [Deconstruction]  [Power, Technique]
  , Equipment "Break of Dawn"  Weapon Radiant [Cunning]  [Power, Mobility]
  , Equipment "Fall of Night"  Weapon Umbral [Cunning]  [Power, Utility]
  , Equipment "Sovereign's Grudge"  Weapon Terra [Sharpened]  [Power, Utility]
  , Equipment "Stalker's Price"  Weapon Umbral [WildFrenzy]  [Technique, Utility]
  , Equipment "Valomyr's Burden"  Weapon Radiant [Aetherhunter]  [Power, Utility]
  , Equipment "Brutality of Boreus"  Weapon Frost [Conditioning]  [Power, Defensive]
  , Equipment "Galvanic Impact"  Weapon Shock [Bond]  [Prismatic, Prismatic]
  ]


helms =
  [ Equipment "Lightning Helm" Helm Shock [] [Technique]
  , Equipment "Skraevwing Helmet" Helm Frost [] [Mobility]
  , Equipment "Shrikedown Helm" Helm Neutral [] [Technique]
  , Equipment "Quillspike Mask" Helm Neutral [] [Defensive]
  , Equipment "Brow of Ice" Helm Frost [] [Power]
  , Equipment "Shocking Gaze" Helm Shock [] [Utility]
  , Equipment "Eye of the Swarm" Helm Terra [] [Mobility]
  , Equipment "Hellplate Casque" Helm Blaze [] [Technique]
  , Equipment "Gnasher Cap" Helm Neutral [] [Defensive]
  , Equipment "Fiery Helm" Helm Blaze [] [Mobility]
  , Equipment "Draskscale Helmet" Helm Shock [] [Power]
  , Equipment "Volcanic Helm" Helm Blaze [] [Defensive]
  , Equipment "Crest of Valour" Helm Radiant [] [Power]
  , Equipment "Guise of the Rift" Helm Umbral [] [Utility]
  , Equipment "Visage of Thorns" Helm Terra [] [Technique]
  , Equipment "Boreal Epiphany" Helm Frost [] [Utility]
  , Equipment "Sight of Stone" Helm Terra [] [Defensive]
  ]

bodyArmors =
  [ Equipment "Lightning Plate" BodyArmor Shock [] [Mobility]
  , Equipment "Skraevwing Jacket" BodyArmor Frost [] [Mobility]
  , Equipment "Shrikedown Plate" BodyArmor Neutral [] [Mobility]
  , Equipment "Quillspike Jacket" BodyArmor Neutral [] [Technique]
  , Equipment "Core of Ice" BodyArmor Frost [] [Power]
  , Equipment "Shocking Heart" BodyArmor Shock [] [Defensive]
  , Equipment "Shell of the Swarm" BodyArmor Terra [] [Technique]
  , Equipment "Hellplate Cuirass" BodyArmor Blaze [] [Technique]
  , Equipment "Gnasher Cloak" BodyArmor Neutral [] [Defensive]
  , Equipment "Fiery Breastplate" BodyArmor Blaze [] [Mobility]
  , Equipment "Draskscale Plate" BodyArmor Shock [] [Power]
  , Equipment "Volcanic Aegis" BodyArmor Blaze [] [Utility]
  , Equipment "Aegis of Valour" BodyArmor Radiant [] [Power]
  , Equipment "Mantle of the Rift" BodyArmor Umbral [] [Utility]
  , Equipment "Mantle of Thorns" BodyArmor Terra [] [Utility]
  , Equipment "Boreal Resolve" BodyArmor Frost [] [Defensive]
  , Equipment "Heart of Stone" BodyArmor Terra [] [Defensive]
  , Equipment "Light's Virtue" BodyArmor Radiant [] []
  ]

gauntlets =
  [ Equipment "Lightning Gloves" Gauntlet Shock [] [Mobility]
  , Equipment "Skraevwing Gloves" Gauntlet Frost [] [Mobility]
  , Equipment "Shrikedown Gloves" Gauntlet Neutral [] [Mobility]
  , Equipment "Quillspike Grips" Gauntlet Neutral [] [Technique]
  , Equipment "Arms of Ice" Gauntlet Frost [] [Defensive]
  , Equipment "Shocking Grasp" Gauntlet Shock [] [Utility]
  , Equipment "Clutches of the Swarm" Gauntlet Terra [] [Mobility]
  , Equipment "Hellplate Bracers" Gauntlet Blaze [] [Power]
  , Equipment "Gnasher Grips" Gauntlet Neutral [] [Power]
  , Equipment "Fiery Gauntlets" Gauntlet Blaze [] [Technique]
  , Equipment "Draskscale Gauntlets" Gauntlet Shock [] [Utility]
  , Equipment "Volcanic Grips" Gauntlet Blaze [] [Utility]
  , Equipment "Gauntlets of Valour" Gauntlet Radiant [] [Defensive]
  , Equipment "Hands of the Rift" Gauntlet Umbral [] [Mobility]
  , Equipment "Grasp of Thorns" Gauntlet Terra [] [Utility]
  , Equipment "Boreal Might" Gauntlet Frost [] [Power]
  , Equipment "Might of Stone" Gauntlet Terra [] [Defensive]
  ]

bootss =
  [ Equipment "Lightning Boots" Boots Shock [] [Mobility]
  , Equipment "Skraevwing Boots" Boots Frost [] [Utility]
  , Equipment "Shrikedown Greaves" Boots Neutral [] [Mobility]
  , Equipment "Quillspike Boots" Boots Neutral [] [Technique]
  , Equipment "Feet of Ice" Boots Frost [] [Defensive]
  , Equipment "Shocking Stride" Boots Shock [] [Defensive]
  , Equipment "Dance of the Swarm" Boots Terra [] [Power]
  , Equipment "Hellplate Greaves" Boots Blaze [] [Power]
  , Equipment "Gnasher Treads" Boots Neutral [] [Power]
  , Equipment "Fiery Greaves" Boots Blaze [] [Defensive]
  , Equipment "Draskscale Greaves" Boots Shock [] [Utility]
  , Equipment "Volcanic Treads" Boots Blaze [] [Power]
  , Equipment "Greaves of Valour" Boots Radiant [] [Technique]
  , Equipment "Striders of the Rift" Boots Umbral [] [Mobility]
  , Equipment "Stride of Thorns" Boots Terra [] [Utility]
  , Equipment "Boreal March" Boots Frost [] [Technique]
  , Equipment "Pillars of Stone" Boots Terra [] [Defensive]
  ]

lanterns =
  [ Equipment "Embermane's Rapture" Lantern Neutral [] [Utility]
  ]
