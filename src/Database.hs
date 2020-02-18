{-# LANGUAGE RecordWildCards,NamedFieldPuns #-}
module Database where

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
  | Nimble
  | Bond

  deriving (Eq,Show)



data Slot
  = Power
  | Mobility
  | Defensive
  | Utility
  | Technique
  | Prismatic
  deriving (Eq)

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


data Resistances = Resistances
  { _blaze :: Int
  , _frost :: Int
  , _shock :: Int
  , _terra :: Int
  , _radiant :: Int
  , _umbral :: Int
  }
  deriving (Eq)

instance Show Resistances where
  show Resistances{..} =
    "blz:" ++ show _blaze ++
    " fro:" ++ show _frost ++
    " sho:" ++ show _shock ++
    " ter:" ++ show _terra ++
    " rad:" ++ show _radiant ++
    " umb:" ++ show _umbral

data Slots = Slots
  { _power :: Int
  , _mobility :: Int
  , _defensive :: Int
  , _utility :: Int
  , _technique :: Int
  , _prismatic :: Int
  }
  deriving (Eq)

instance Show Slots where
  show Slots{..} =
    "pow:" ++ show _power ++
    " mob:" ++ show _mobility ++
    " def:" ++ show _defensive ++
    " utl:" ++ show _utility ++
    " tec:" ++ show _technique ++
    " pri:" ++ show _prismatic

noSlots = Slots 0 0 0 0 0 0

neutralResistances = Resistances 0 0 0 0 0 0

resistance :: Element -> Resistances -> Resistances
resistance Neutral r = r
resistance Blaze r@Resistances{_blaze=blaze,_frost=frost} = r { _blaze = blaze+1, _frost = frost-1 }
resistance Frost r@Resistances{_blaze=blaze,_frost=frost} = r { _blaze = blaze-1, _frost = frost+1 }
resistance Shock r@Resistances{_shock=shock,_terra=terra} = r { _shock = shock+1, _terra = terra-1 }
resistance Terra r@Resistances{_shock=shock,_terra=terra} = r { _shock = shock-1, _terra = terra+1 }
resistance Radiant r@Resistances{_radiant=radiant,_umbral=umbral} = r { _radiant = radiant+1, _umbral = umbral-1 }
resistance Umbral r@Resistances{_radiant=radiant,_umbral=umbral} = r { _radiant = radiant-1, _umbral = umbral+1 }

advantage :: Element -> Resistances -> Resistances
advantage Neutral r = r
advantage Blaze r@Resistances{_blaze=blaze,_frost=frost} = r { _blaze = blaze-1, _frost = frost+1 }
advantage Frost r@Resistances{_blaze=blaze,_frost=frost} = r { _blaze = blaze+1, _frost = frost-1 }
advantage Shock r@Resistances{_shock=shock,_terra=terra} = r { _shock = shock-1, _terra = terra+1 }
advantage Terra r@Resistances{_shock=shock,_terra=terra} = r { _shock = shock+1, _terra = terra-1 }
advantage Radiant r@Resistances{_radiant=radiant,_umbral=umbral} = r { _radiant = radiant-1, _umbral = umbral+1 }
advantage Umbral r@Resistances{_radiant=radiant,_umbral=umbral} = r { _radiant = radiant+1, _umbral = umbral-1 }

equipmentProperty :: (Element -> Resistances -> Resistances) -> Equipment -> Resistances -> Resistances
equipmentProperty prop Equipment{_element} = prop _element

loadoutResistances :: Loadout -> Resistances
loadoutResistances Loadout{..} =
  helmRes $ bodyRes $ gauntletRes $ bootsRes neutralResistances
  where
  helmRes = equipmentProperty resistance _helm
  bodyRes = equipmentProperty resistance _bodyArmor
  gauntletRes = equipmentProperty resistance _gauntlet
  bootsRes = equipmentProperty resistance _boots

loadoutAdvantage :: Loadout -> Resistances
loadoutAdvantage Loadout{_weapon} = (equipmentProperty advantage _weapon) neutralResistances

slots :: Slot -> Slots -> Slots
slots Power      s@Slots{_power}     = s { _power=_power+1 }
slots Mobility   s@Slots{_mobility}  = s { _mobility=_mobility+1 }
slots Defensive  s@Slots{_defensive} = s { _defensive=_defensive+1 }
slots Utility    s@Slots{_utility}   = s { _utility=_utility+1 }
slots Technique  s@Slots{_technique} = s { _technique=_technique+1 }
slots Prismatic  s@Slots{_prismatic} = s { _prismatic=_prismatic+1 }

equipmentSlots :: Equipment -> Slots -> Slots
equipmentSlots Equipment{_slots} s = (foldl (.) id (map slots _slots)) s

loadoutSlots :: Loadout -> Slots
loadoutSlots (Loadout {..}) = allSlots
  where
    allPerks = concat
      [ _perks _weapon
      , _perks _helm
      , _perks _bodyArmor
      , _perks _gauntlet
      , _perks _boots
      , _perks _lantern
      ]
    allSlots = weaponSlots $ helmSlots $ bodySlots $ gauntletSlots $ lanternSlots noSlots
    weaponSlots = equipmentSlots _weapon
    helmSlots = equipmentSlots _helm
    bodySlots = equipmentSlots _bodyArmor
    gauntletSlots = equipmentSlots _gauntlet
    lanternSlots = equipmentSlots _lantern

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
