{-# LANGUAGE RecordWildCards,NamedFieldPuns #-}
module Types where

import qualified Data.Map as Map
import Data.Map (Map)

import Histogram

data Equipment = Equipment
  { _name :: String
  , _type :: EType
  , _element :: Element
  , _perks :: [Perk]
  , _slots :: [Slot]
  }
  deriving (Eq)

instance Show Equipment where
  show (Equipment{..}) = _name

data Element
  = Neutral
  | Blaze
  | Frost
  | Shock
  | Terra
  | Radiant
  | Umbral
  deriving (Eq,Ord)

instance Show Element where
  show Neutral = "ntr"
  show Blaze = "blz"
  show Frost = "fro"
  show Shock = "sho"
  show Terra = "ter"
  show Radiant = "rad"
  show Umbral = "umb"

data EType
  = Helm
  | BodyArmor
  | Gauntlet
  | Boots
  | Lantern
  | Weapon
  deriving (Eq,Show)

data Perk
  -- Defensive
  = Fireprof
  | AssassinsVigour
  | NineLives
  | Tough
  | Warmth
  | Bloodless
  | Fortress
  | Sturdy

  -- Power
  | Ragehunter
  | Sharpened
  | Aetherhunter
  | Deconstruction
  | KnockoutKing
  | Overpower
  | Pacifier
  
  -- Mobility
  | Agility
  | Conditioning
  | Swift
  | Endurance
  | Evasion
  | FleetFooted
  
  -- Technique
  | Cunning
  | Savagery
  | Acidic
  | Adrenaline
  | Barbed
  | Bladestorm
  | EvasiveFury
  | Molten
  | Predator
  | WildFrenzy

  -- Utility
  | Aetherborne
  | AethericAttunement
  | AethericFrenzy
  | Conduit
  | Energized
  | Medic
  | Lucent
  | StunningVigour

  -- TODO
  | Insulated
  | Nimble
  | Bond

  deriving (Eq,Show,Ord)



data Slot
  = Power
  | Mobility
  | Defensive
  | Utility
  | Technique
  | Prismatic
  deriving (Eq,Ord)

instance Show Slot where
  show Power     = "pow"
  show Mobility  = "mob"
  show Defensive = "def"
  show Utility   = "utl"
  show Technique = "tec"
  show Prismatic = "pri"



data Loadout = Loadout
  { _weapon :: Equipment
  , _helm :: Equipment
  , _bodyArmor :: Equipment
  , _gauntlet :: Equipment
  , _boots :: Equipment
  , _lantern :: Equipment
  }
  deriving (Eq)


instance Show Loadout where
  show Loadout {..} =
    show _weapon ++ "\n" ++
    show _helm ++ "\n" ++
    show _bodyArmor ++ "\n" ++
    show _gauntlet ++ "\n" ++
    show _boots ++ "\n" ++
    show _lantern


type Slots = Histogram Slot
type Resistances = Histogram Element
type Perks = Histogram Perk


resistance :: Element -> Resistances -> Resistances
resistance Neutral = id
resistance Blaze = inc Blaze . dec Frost
resistance Frost = inc Frost . dec Blaze
resistance Shock = inc Shock . dec Terra
resistance Terra = inc Terra . dec Shock
resistance Radiant = inc Radiant . dec Umbral
resistance Umbral = inc Umbral . dec Radiant

equipmentResistance :: Equipment -> Resistances -> Resistances
equipmentResistance Equipment{_element} = resistance _element

loadoutResistances :: Loadout -> Resistances
loadoutResistances Loadout{..} =
  helmRes $ bodyRes $ gauntletRes $ bootsRes mempty
  where
  helmRes = equipmentResistance _helm
  bodyRes = equipmentResistance _bodyArmor
  gauntletRes = equipmentResistance _gauntlet
  bootsRes = equipmentResistance _boots

loadoutAdvantage :: Loadout -> Resistances
loadoutAdvantage Loadout{_weapon} = (equipmentResistance _weapon) mempty

equipmentSlots :: Equipment -> Slots -> Slots
equipmentSlots Equipment{_slots} s = foldl (flip id) s (map inc _slots)

loadoutSlots :: Loadout -> Slots
loadoutSlots (Loadout {..}) = allSlots
  where
    allSlots = weaponSlots $ helmSlots $ bodySlots $ gauntletSlots $ bootsSlots $ lanternSlots mempty
    weaponSlots = equipmentSlots _weapon
    helmSlots = equipmentSlots _helm
    bodySlots = equipmentSlots _bodyArmor
    gauntletSlots = equipmentSlots _gauntlet
    bootsSlots = equipmentSlots _boots
    lanternSlots = equipmentSlots _lantern

loadoutPerks :: Loadout -> Perks
loadoutPerks (Loadout {..}) = allPerks
  where
    allPerks = foldl (flip id) mempty $ map inc $ concat
      [ _perks _weapon
      , _perks _helm
      , _perks _bodyArmor
      , _perks _gauntlet
      , _perks _boots
      , _perks _lantern
      ]
