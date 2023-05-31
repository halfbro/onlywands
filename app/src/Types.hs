module Types
  ( Inventory,
    SpellName,
    StreamerInformation,
    blankStreamerInformation,
    fromNoitaUpdate,
    Wand,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Noita.Types as Noita
import System.FilePath.Posix

type SpellName = String

type Inventory = [SpellName]

data WandStats = WandStats
  { sprite :: String,
    rechargeTime :: Integer,
    manaRechargeRate :: Double,
    spreadDegrees :: Double,
    isShuffle :: Bool,
    manaMax :: Double,
    spellsPerCast :: Integer,
    speedMultiplier :: Double,
    baseCastDelay :: Integer,
    wandCapacity :: Integer
  }
  deriving (Generic, Show, Eq)

instance ToJSON WandStats

instance FromJSON WandStats

fromNoitaWandStats :: NoitaWandStats -> WandStats
fromNoitaWandStats stats =
  let fromNoitaSprite = takeBaseName
   in WandStats
        { sprite = fromNoitaSprite $ Noita.sprite stats,
          rechargeTime = Noita.reload_time stats,
          manaRechargeRate = Noita.mana_charge_speed stats,
          spreadDegrees = Noita.spread_degrees stats,
          isShuffle = Noita.shuffle_deck_when_empty stats,
          manaMax = Noita.mana_max stats,
          spellsPerCast = Noita.actions_per_round stats,
          speedMultiplier = Noita.speed_multiplier stats,
          baseCastDelay = Noita.fire_rate_wait stats,
          wandCapacity = Noita.deck_capacity stats
        }

data Wand = Wand
  { stats :: WandStats,
    alwaysCast :: [SpellName],
    deck :: [SpellName]
  }
  deriving (Generic, Show, Eq)

instance ToJSON Wand

instance FromJSON Wand

fromNoitaWand :: (NoitaWandStats, [NoitaSpellName], [NoitaSpellName]) -> Wand
fromNoitaWand (stats, alwaysCast, deck) =
  Wand
    { stats = fromNoitaWandStats stats,
      alwaysCast = alwaysCast,
      deck = deck
    }

data StreamerInformation = StreamerInformation
  { wands :: [Wand],
    inventory :: Inventory
  }
  deriving (Generic, Show, Eq)

instance ToJSON StreamerInformation

instance FromJSON StreamerInformation

blankStreamerInformation :: StreamerInformation
blankStreamerInformation = StreamerInformation {wands = [], inventory = []}

fromNoitaUpdate :: NoitaUpdate -> StreamerInformation
fromNoitaUpdate update =
  StreamerInformation
    { wands = List.map fromNoitaWand $ Noita.wands update,
      inventory = fromMaybe [] $ Noita.inventory update
    }
