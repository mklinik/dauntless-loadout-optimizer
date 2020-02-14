{-# LANGUAGE RecordWildCards #-}
module Database where

data Equipment = Equipment
  { name :: String
  , typ :: EType
  , perk :: Perk
  , slots :: [Slot]
  }
  deriving (Eq,Show)

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

  deriving (Eq,Show)


data Slot
  = Power
  | Mobility
  | Defensive
  | Utility
  | Technique
  deriving (Eq,Show)


data Loadout = Loadout
  { weapon :: Equipment
  , helm :: Equipment
  , bodyArmor :: Equipment
  , gauntlet :: Equipment
  , boots :: Equipment
  , lantern :: Equipment
  }
  deriving (Eq,Show)


loadoutSlots :: Loadout -> ([Perk], [Slot])
loadoutSlots (Loadout {..}) = (allPerks,allSlots)
  where
    allPerks =
      [ perk weapon
      , perk helm
      , perk bodyArmor
      , perk gauntlet
      , perk boots
      , perk lantern
      ]
    allSlots = concat
      [ slots weapon
      , slots helm
      , slots bodyArmor
      , slots gauntlet
      , slots lantern
      ]
