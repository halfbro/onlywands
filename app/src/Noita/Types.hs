module Noita.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

{-
[
  {
    "sprite": "data\\/items_gfx\\/wands\\/wand_0821.png",
    "reload_time": 20,
    "mana_charge_speed": 2054,
    "spread_degrees": -30,
    "shuffle_deck_when_empty": false,
    "ui_name": "Bolt staff",
    "mana_max": 2433,
    "actions_per_round": 1,
    "speed_multiplier": 1,
    "fire_rate_wait": 10,
    "deck_capacity": 26
  },
  [],
  [
    "LIGHT_BULLET"
  ]
]

{
  "inventory": [
    "LASER_EMITTER_FOUR",
    "0",
    "0",
    "0",
    "0",
    "LIGHT_BULLET_TRIGGER",
    "MIST_BLOOD",
    "SPORE_POD",
    "LIGHT_BULLET_TRIGGER_2",
    "POWERDIGGER",
    "LIGHT_BULLET",
    "HOMING",
    "HEAL_BULLET"
  ],
  "wands": [
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/custom\\/good_01.xml",
        "reload_time": 5,
        "mana_charge_speed": 601,
        "spread_degrees": 0,
        "shuffle_deck_when_empty": false,
        "ui_name": "Fast wand",
        "mana_max": 947,
        "actions_per_round": 1,
        "speed_multiplier": 1.25,
        "fire_rate_wait": -20,
        "deck_capacity": 19
      },
      [],
      [
        "SLOW_BULLET"
      ]
    ],
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/custom\\/good_02.xml",
        "reload_time": -2280,
        "mana_charge_speed": 800,
        "spread_degrees": 0,
        "shuffle_deck_when_empty": false,
        "ui_name": "Destructive wand",
        "mana_max": 1498,
        "actions_per_round": 1,
        "speed_multiplier": 1.3300000429153442,
        "fire_rate_wait": 30,
        "deck_capacity": 26
      },
      [
        "NUKE"
      ],
      [
        "ROCKET_TIER_3",
        "ROCKET_TIER_3",
        "ROCKET_TIER_3",
        "ROCKET_TIER_3"
      ]
    ],
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/wand_0821.png",
        "reload_time": 20,
        "mana_charge_speed": 2054,
        "spread_degrees": -30,
        "shuffle_deck_when_empty": false,
        "ui_name": "Rapid bolt wand",
        "mana_max": 2433,
        "actions_per_round": 1,
        "speed_multiplier": 2,
        "fire_rate_wait": 10,
        "deck_capacity": 26
      },
      [],
      [
        "BURST_4",
        "DIVIDE_10",
        "DIVIDE_4",
        "DIVIDE_3",
        "ADD_TRIGGER",
        "DIVIDE_3",
        "DIVIDE_2",
        "ADD_TRIGGER",
        "ADD_TRIGGER",
        "PHASING_ARC",
        "PHASING_ARC",
        "LINE_ARC",
        "BLOOD_MAGIC",
        "ADD_DEATH_TRIGGER",
        "HOMING_SHOOTER",
        "X_RAY",
        "BURST_2",
        "DAMAGE",
        "SWAPPER_PROJECTILE",
        "EXPLODING_DEER"
      ]
    ],
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/wand_0821.png",
        "reload_time": 2,
        "mana_charge_speed": 5000,
        "spread_degrees": -30,
        "shuffle_deck_when_empty": false,
        "ui_name": "Rapid bolt wand",
        "mana_max": 5000,
        "actions_per_round": 1,
        "speed_multiplier": 2,
        "fire_rate_wait": 0,
        "deck_capacity": 26
      },
      [],
      [
        "0",
        "0",
        "MANA_REDUCE",
        "BURST_2",
        "LONG_DISTANCE_CAST",
        "TAU",
        "NOLLA",
        "BLACK_HOLE",
        "TELEPORT_PROJECTILE_SHORT",
        "CHAINSAW"
      ]
    ]
  ]
}

{
  "inventory": [
    "LASER_EMITTER_FOUR",
    "0",
    "0",
    "0",
    "0",
    "LIGHT_BULLET_TRIGGER",
    "MIST_BLOOD",
    "SPORE_POD",
    "LIGHT_BULLET_TRIGGER_2",
    "POWERDIGGER",
    "LIGHT_BULLET",
    "HOMING",
    "HEAL_BULLET"
  ],
  "wands": [
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/wand_0821.png",
        "reload_time": 20,
        "mana_charge_speed": 2054,
        "spread_degrees": -30,
        "shuffle_deck_when_empty": false,
        "ui_name": "Bolt staff",
        "mana_max": 2433,
        "actions_per_round": 1,
        "speed_multiplier": 1,
        "fire_rate_wait": 10,
        "deck_capacity": 26
      },
      [],
      [
        "LIGHT_BULLET"
      ]
    ],
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/custom\\/good_02.xml",
        "reload_time": -2280,
        "mana_charge_speed": 800,
        "spread_degrees": 0,
        "shuffle_deck_when_empty": false,
        "ui_name": "Destructive wand",
        "mana_max": 1498,
        "actions_per_round": 1,
        "speed_multiplier": 1.3300000429153442,
        "fire_rate_wait": 30,
        "deck_capacity": 26
      },
      [
        "NUKE"
      ],
      [
        "ROCKET_TIER_3",
        "ROCKET_TIER_3",
        "ROCKET_TIER_3",
        "ROCKET_TIER_3"
      ]
    ],
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/wand_0821.png",
        "reload_time": 20,
        "mana_charge_speed": 2054,
        "spread_degrees": -30,
        "shuffle_deck_when_empty": false,
        "ui_name": "Rapid bolt wand",
        "mana_max": 2433,
        "actions_per_round": 1,
        "speed_multiplier": 2,
        "fire_rate_wait": 10,
        "deck_capacity": 26
      },
      [],
      [
        "BURST_4",
        "DIVIDE_10",
        "DIVIDE_4",
        "DIVIDE_3",
        "ADD_TRIGGER",
        "DIVIDE_3",
        "DIVIDE_2",
        "ADD_TRIGGER",
        "ADD_TRIGGER",
        "PHASING_ARC",
        "PHASING_ARC",
        "LINE_ARC",
        "BLOOD_MAGIC",
        "ADD_DEATH_TRIGGER",
        "HOMING_SHOOTER",
        "X_RAY",
        "BURST_2",
        "DAMAGE",
        "SWAPPER_PROJECTILE",
        "EXPLODING_DEER"
      ]
    ],
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/wand_0821.png",
        "reload_time": 2,
        "mana_charge_speed": 5000,
        "spread_degrees": -30,
        "shuffle_deck_when_empty": false,
        "ui_name": "Rapid bolt wand",
        "mana_max": 5000,
        "actions_per_round": 1,
        "speed_multiplier": 2,
        "fire_rate_wait": 0,
        "deck_capacity": 26
      },
      [],
      [
        "0",
        "0",
        "MANA_REDUCE",
        "BURST_2",
        "LONG_DISTANCE_CAST",
        "TAU",
        "NOLLA",
        "BLACK_HOLE",
        "TELEPORT_PROJECTILE_SHORT",
        "CHAINSAW"
      ]
    ]
  ]
}



{
  "inventory": [
    "LASER_EMITTER_FOUR",
    "HOMING",
    "LIGHT_BULLET",
    "0",
    "0",
    "MIST_BLOOD",
    "POWERDIGGER",
    "SPORE_POD",
    "0",
    "LIGHT_BULLET_TRIGGER_2"
  ],
  "wands": [
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/wand_0821.png",
        "reload_time": 20,
        "mana_charge_speed": 2054,
        "spread_degrees": -30,
        "shuffle_deck_when_empty": false,
        "ui_name": "Bolt staff",
        "mana_max": 2433,
        "actions_per_round": 1,
        "speed_multiplier": 1,
        "fire_rate_wait": 10,
        "deck_capacity": 26
      },
      [],
      [
        "LIGHT_BULLET"
      ]
    ],
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/custom\\/good_03.xml",
        "reload_time": 32,
        "mana_charge_speed": 424,
        "spread_degrees": 3,
        "shuffle_deck_when_empty": false,
        "ui_name": "Scattershot wand",
        "mana_max": 1425,
        "actions_per_round": 26,
        "speed_multiplier": 1.5,
        "fire_rate_wait": 7,
        "deck_capacity": 26
      },
      [],
      [
        "DISC_BULLET",
        "DISC_BULLET",
        "DISC_BULLET",
        "DISC_BULLET",
        "DISC_BULLET",
        "DISC_BULLET",
        "DISC_BULLET",
        "DISC_BULLET"
      ]
    ],
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/wand_0821.png",
        "reload_time": 20,
        "mana_charge_speed": 2054,
        "spread_degrees": -30,
        "shuffle_deck_when_empty": false,
        "ui_name": "Rapid bolt wand",
        "mana_max": 2433,
        "actions_per_round": 1,
        "speed_multiplier": 2,
        "fire_rate_wait": 10,
        "deck_capacity": 26
      },
      [],
      [
        "LIGHT_BULLET_TRIGGER",
        "HEAL_BULLET"
      ]
    ],
    [
      {
        "sprite": "data\\/items_gfx\\/wands\\/wand_0821.png",
        "reload_time": 2,
        "mana_charge_speed": 5000,
        "spread_degrees": -30,
        "shuffle_deck_when_empty": false,
        "ui_name": "Rapid bolt wand",
        "mana_max": 5000,
        "actions_per_round": 1,
        "speed_multiplier": 2,
        "fire_rate_wait": 0,
        "deck_capacity": 26
      },
      [],
      [
        "0",
        "0",
        "MANA_REDUCE",
        "BURST_2",
        "LONG_DISTANCE_CAST",
        "TAU",
        "NOLLA",
        "BLACK_HOLE",
        "TELEPORT_PROJECTILE_SHORT",
        "CHAINSAW"
      ]
    ]
  ]
}
-}

data NoitaUpdate = NoitaUpdate
  { inventory :: Maybe NoitaInventory,
    wands :: [(NoitaWandStats, [NoitaSpellName], [NoitaSpellName])]
  }
  deriving (Generic, Show, Eq)

instance FromJSON NoitaUpdate

type NoitaSpellName = String

type NoitaInventory = [NoitaSpellName]

type NoitaSprite = String

data NoitaWandStats = NoitaWandStats
  { sprite :: NoitaSprite,
    reload_time :: Integer,
    mana_charge_speed :: Double,
    spread_degrees :: Double,
    shuffle_deck_when_empty :: Bool,
    ui_name :: Maybe String, -- May actually be a string for special wands
    mana_max :: Double,
    actions_per_round :: Integer,
    speed_multiplier :: Double,
    fire_rate_wait :: Integer,
    deck_capacity :: Integer
  }
  deriving (Generic, Show, Eq)

instance FromJSON NoitaWandStats
