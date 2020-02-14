{-# LANGUAGE RecordWildCards #-}
module Database where

data Equipment = Equipment
  { name :: String
  , typ :: EType
  , perks :: [Perk]
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

  -- TODO
  | Insulated

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
    allPerks = concat
      [ perks weapon
      , perks helm
      , perks bodyArmor
      , perks gauntlet
      , perks boots
      , perks lantern
      ]
    allSlots = concat
      [ slots weapon
      , slots helm
      , slots bodyArmor
      , slots gauntlet
      , slots lantern
      ]

weapons =
  [ Equipment "Fall of the Shrike" Weapon [Conditioning] [Power, Mobility]
  , Equipment "Ember Maul"         Weapon [EvasiveFury]  [Power, Mobility]
  ]

helms =
  [ Equipment "Boreal Epiphany" Helm [Conditioning] [Utility]
  , Equipment "Fiery Helm" Helm [EvasiveFury] [Mobility]
  ]

bodyArmors =
  [ Equipment "Gnasher Cloak" BodyArmor [Tough] [Defensive]
  , Equipment "Fiery Breastplate" BodyArmor [EvasiveFury] [Mobility]
  ]

gauntlets =
  [ Equipment "Shocking Grasp" Gauntlet [AethericAttunement] [Utility]
  , Equipment "Fiery Gauntlets" Gauntlet [Fireprof] [Technique]
  ]

bootss =
  [ Equipment "Shocking Stride" Boots [Insulated] [Defensive]
  , Equipment "Fiery Greaves" Boots [Evasion] [Defensive]
  ]

lanterns =
  [ Equipment "Embermane's Rapture" Lantern [] [Utility]
  ]
