module Types exposing (..)

import Dict exposing (Dict)
import Json.Decode as JD
    exposing
        ( Decoder
        , bool
        , float
        , int
        , list
        , string
        )
import Json.Decode.Pipeline exposing (hardcoded, optional, required)


type alias WandInformation =
    { wands : List String
    , inventory : List String
    }


type alias SpellName =
    String


type alias Inventory =
    List SpellName


type alias WandStats =
    { sprite : String
    , rechargeTime : Int
    , manaRechargeRate : Float
    , spreadDegrees : Float
    , isShuffle : Bool
    , manaMax : Float
    , spellsPerCast : Int
    , speedMultiplier : Float
    , baseCastDelay : Int
    , wandCapacity : Int
    }


decodeWandStats : Decoder WandStats
decodeWandStats =
    JD.succeed WandStats
        |> required "sprite" string
        |> required "rechargeTime" int
        |> required "manaRechargeRate" float
        |> required "spreadDegrees" float
        |> required "isShuffle" bool
        |> required "manaMax" float
        |> required "spellsPerCast" int
        |> required "speedMultiplier" float
        |> required "baseCastDelay" int
        |> required "wandCapacity" int


type alias Wand =
    { stats : WandStats
    , alwaysCast : List SpellName
    , deck : List SpellName
    }


decodeWand : Decoder Wand
decodeWand =
    JD.succeed Wand
        |> required "stats" decodeWandStats
        |> required "alwaysCast" (list string)
        |> required "deck" (list string)


type alias StreamerInformation =
    { wands : List Wand
    , inventory : Inventory
    }


blankInfo : StreamerInformation
blankInfo =
    { wands = [], inventory = [] }


decodeUpdate : Decoder StreamerInformation
decodeUpdate =
    JD.succeed StreamerInformation
        |> required "wands" (list decodeWand)
        |> required "inventory" (list string)


type SpellType
    = Projectile -- 0
    | StaticProjectile -- 1
    | Modifier -- 2
    | Multicast -- 3
    | Material -- 4
    | Other -- 5
    | Utility -- 6
    | Passive -- 7
    | Custom Int


spellTypeFromEnum : Int -> SpellType
spellTypeFromEnum i =
    case i of
        0 ->
            Projectile

        1 ->
            StaticProjectile

        2 ->
            Modifier

        3 ->
            Multicast

        4 ->
            Material

        5 ->
            Other

        6 ->
            Utility

        7 ->
            Passive

        x ->
            Custom x


spellTypeToString : SpellType -> String
spellTypeToString t =
    case t of
        Projectile ->
            "Projectile"

        StaticProjectile ->
            "Static proj."

        Modifier ->
            "Proj. modifier"

        Multicast ->
            "Multicast"

        Material ->
            "Material"

        Other ->
            "Other"

        Utility ->
            "Utility"

        Passive ->
            "Passive"

        Custom x ->
            String.fromInt x


decodeSpellType : Decoder SpellType
decodeSpellType =
    int
        |> JD.map spellTypeFromEnum



--
{- :
   type SpellAttribute
       = Type SpellType -- action_type
       | ManaDrain Int -- action_mana_drain
       | CastDelay Int -- fire_rate_wait
       | RechargeDelay Int -- reload_time
       | Spread Float -- spread_degrees
       | DamageProjectile Int -- Not in old data set
       | DamageFire Int -- Not in old data set
       | DamageDrill Int -- Not in old data set
       | DamageSlice Int -- Not in old data set
       | DamageElectric Int -- Not in old data set
       | DamageExplosion Int -- Not in old data set
       | ExplosionRadius Int -- explosion_radius
       | SpeedMultiplier Float -- speed_multiplier
       | MaxUses Int -- action_max_uses
       | Bounces Int -- bounces -- Is this still used?
       | CritChance Int -- damage_critical_chance
       | Raw String Float
   :
-}
--


type alias SpellAttributes =
    { spellType : SpellType -- action_type
    , manaDrain : Int -- action_mana_drain
    , castDelay : Maybe Int -- fire_rate_wait
    , rechargeDelay : Maybe Int -- reload_time
    , spread : Maybe Float -- spread_degrees
    , damageProjectile : Maybe Int -- Not in old data set
    , damageFire : Maybe Int -- Not in old data set
    , damageDrill : Maybe Int -- Not in old data set
    , damageSlice : Maybe Int -- Not in old data set
    , damageElectric : Maybe Int -- Not in old data set
    , damageExplosion : Maybe Int -- Not in old data set
    , explosionRadius : Maybe Int -- explosion_radius
    , speedMultiplier : Maybe Float -- speed_multiplier
    , maxUses : Maybe Int -- action_max_uses
    , bounces : Maybe Int -- bounces
    , critChance : Maybe Int -- damage_critical_chance
    , other : Dict String Float
    }


decodeSpellAttributes : Decoder SpellAttributes
decodeSpellAttributes =
    let
        missing field d =
            optional field (d |> JD.map Just) Nothing
    in
    JD.succeed SpellAttributes
        |> required "action_type" decodeSpellType
        |> required "action_mana_drain" int
        |> missing "fire_rate_wait" int
        |> missing "reload_time" int
        |> missing "spread_degrees" float
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> missing "explosion_radius" int
        |> missing "speed_multiplier" float
        |> missing "action_max_uses" int
        |> missing "bounces" int
        |> missing "damage_critical_chance" int
        |> hardcoded Dict.empty


type alias Spell =
    { name : String
    , description : String
    , meta : SpellAttributes
    , sprite : String
    }


type alias SpellData =
    Dict SpellName Spell


type alias WandSprites =
    Dict String String


decodeSpell : Decoder Spell
decodeSpell =
    JD.succeed Spell
        |> required "name" string
        |> required "description" string
        |> required "meta" decodeSpellAttributes
        |> required "sprite" string
