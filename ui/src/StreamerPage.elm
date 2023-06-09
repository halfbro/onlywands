port module StreamerPage exposing (main)

import Browser
import Css
import Dict exposing (empty)
import Html.Styled exposing (Html, a, div, img, p, span, styled, table, td, text, toUnstyled, tr)
import Html.Styled.Attributes exposing (class, href, src)
import Json.Decode exposing (Error, decodeString, decodeValue, dict, string)
import List exposing (length)
import Result exposing (withDefault)
import Round
import String exposing (fromInt)
import Types exposing (..)


port streamerInfoUpdates : (String -> msg) -> Sub msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = toUnstyled << view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ streamerInfoUpdates
            (\str ->
                case decodeString decodeUpdate str of
                    Ok i ->
                        ReceivedWandUpdate i

                    Err e ->
                        BadWandUpdate e
            )
        ]


type alias Flags =
    { streamerName : String
    , spellData : Json.Decode.Value
    , wandSprites : Json.Decode.Value
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        m =
            Model
                blankInfo
                flags.streamerName
                (withDefault empty <| decodeValue (dict decodeSpell) flags.spellData)
                (withDefault empty <| decodeValue (dict string) flags.wandSprites)
    in
    ( m, Cmd.none )


type Msg
    = ReceivedWandUpdate StreamerInformation
    | BadWandUpdate Error


type alias Model =
    { streamerInfo : StreamerInformation
    , route : String
    , spellData : SpellData
    , wandSprites : WandSprites
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ReceivedWandUpdate info ->
            ( { model | streamerInfo = info }, Cmd.none )

        BadWandUpdate e ->
            let
                _ =
                    Debug.log "Unexpected Wand Update error" e
            in
            ( model, Cmd.none )


view : Model -> Html msg
view model =
    let
        viewAllWandDetails =
            styled div
                [ Css.displayFlex
                , Css.flexFlow2 Css.row Css.wrap
                , Css.justifyContent Css.center
                ]
                []
                (List.map (viewWandDetails model.spellData model.wandSprites) model.streamerInfo.wands)
    in
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.column
        ]
        []
        [ viewInventory model.spellData model.streamerInfo.inventory
        , viewAllWandDetails
        , styled span [ Css.textAlign Css.center ] [] [ text "Disclaimer: Spell tooltips are not accurate at the moment." ]
        , styled span
            [ Css.textAlign Css.center ]
            []
            [ a [ href "/" ] [ text "Add to your stream" ]
            ]
        ]


viewInventory : SpellData -> Inventory -> Html msg
viewInventory spellData inventory =
    let
        inventoryPadding =
            List.repeat (16 - length inventory) "0"
    in
    styled div
        [ Css.backgroundColor <| Css.rgb 55 39 36
        , Css.border3 (Css.px 3) Css.solid <| Css.rgb 55 39 36
        , Css.borderRadius (Css.px 3)
        , Css.maxWidth Css.maxContent
        , Css.margin Css.auto
        ]
        []
        [ viewSpellDeck spellData <| inventory ++ inventoryPadding ]



{- :
   viewWandBrief : SpellData -> WandSprites -> Wand -> Html msg
   viewWandBrief spellData wandSprites wand =
       let
           deckPadding =
               List.repeat (wand.stats.deckCapacity - length wand.alwaysCast - length wand.deck) "0"
       in
       div [ class "wand-brief" ]
           [ div [ class "wand-brief-upper" ]
               [ viewWandSprite wandSprites wand False
               , table []
                   [ tr []
                       [ td [ class "stat" ] [ text "Shuffle" ]
                       , td []
                           [ text <|
                               if wand.stats.shuffleDeckWhenEmpty then
                                   "Yes"

                               else
                                   "No"
                           ]
                       ]
                   , tr []
                       [ td [ class "stat" ] [ text "Spells/Cast" ]
                       , td []
                           [ text <| fromInt wand.stats.actionsPerRound ]
                       ]
                   ]
               ]
           , viewSpellDeck spellData (wand.deck ++ deckPadding)
           , div [ class "wand-detail" ] [ viewWandDetails spellData wandSprites wand ]
           ]
   :
-}
--


viewWandSprite : WandSprites -> Wand -> Bool -> Html msg
viewWandSprite wandSprites wand isRotated =
    let
        imageBase64 =
            "data:image/png;base64, " ++ (Dict.get wand.stats.sprite wandSprites |> Maybe.withDefault "")
    in
    styled img
        [ Css.width (Css.px 110)
        , Css.alignSelf Css.center
        , Css.transform <|
            Css.rotate
                (Css.deg
                    (if isRotated then
                        -90

                     else
                        0
                    )
                )
        ]
        [ src imageBase64 ]
        []


viewWandDetails : SpellData -> WandSprites -> Wand -> Html msg
viewWandDetails spellData wandSprites wand =
    let
        statsSection =
            viewWandStats wand

        alwaysCastSection =
            viewWandAlwaysCast spellData wand

        deckPadding =
            List.repeat (wand.stats.wandCapacity - length wand.alwaysCast - length wand.deck) "0"

        deckSection =
            div []
                [ p [] [ text "Spells:" ]
                , viewSpellDeck spellData (wand.deck ++ deckPadding)
                ]
    in
    styled div
        [ Css.maxWidth Css.minContent
        , Css.minHeight Css.fitContent
        , Css.padding (Css.px 5)
        , Css.margin (Css.px 5)
        , Css.border3 (Css.px 2) Css.solid (Css.rgb 255 255 255)
        , Css.borderRadius (Css.px 2)
        , Css.backgroundColor (Css.rgb 17 13 12)
        ]
        []
        [ styled div
            [ Css.displayFlex
            , Css.minWidth Css.maxContent
            , Css.paddingBottom (Css.px 20)
            ]
            []
            [ statsSection, viewWandSprite wandSprites wand True ]
        , alwaysCastSection
        , deckSection
        ]


showTimeInteger : Int -> String
showTimeInteger i =
    (Round.round 2 <| (toFloat i * 5 / 300)) ++ " s"


viewWandStats : Wand -> Html msg
viewWandStats wand =
    let
        stats =
            wand.stats

        shuffleIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAANUlEQVR4nG2NQQqAQAzEEvD/X46HurqKPYWBpAJANaBefE/DlZX6Mf7PvfgYE9q7wPH6u4wThCMm8HHIUdIAAAAASUVORK5CYII="

        spellsPerCastIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAALUlEQVR4nH3MMQ4AIAzDQJv//zkMMFQt4sYoskkoVGDxYvse/fsrXLWTxLkCG+ynDwFoQL9BAAAAAElFTkSuQmCC"

        castDelayIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAAFElEQVQImWNgoB34////f2w0jQAAvkUL9V/xo/8AAAAASUVORK5CYII="

        rechargeTimeIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAAL0lEQVR4nG2KsQ0AMAzC7P7/Mx0ipVUUJmMQAJIUqAz1+Fc9HLac8VpKkmK7qz1f19Yg7L/pwAsAAAAASUVORK5CYII="

        manaMaxIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAALElEQVQImWNgQAL/////j8xnxCbByMjICJdE1wFTwIQuiAzwSuK1EwWg2w0AonYUALgX+aEAAAAASUVORK5CYII="

        manaRechargeIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAALUlEQVQImWNgwAL+////n4GBgYERlwROQRibCZmDrosJm514JRkZGVHdgs0hANHrG/AaxgVNAAAAAElFTkSuQmCC"

        capacityIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAAKElEQVR4nGNkYGD4//8/AxJgZGRkRBOCACZMIQZM7RAR7CbgUIvVDQDKEhf1CwHbjAAAAABJRU5ErkJggg=="

        spreadIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAAH0lEQVR4nGNkQAX///9H56MIofOxaMGnFqscI1Y3AAD/ySDi7uHLHwAAAABJRU5ErkJggg=="

        withStatIcon image =
            Css.before
                [ Css.marginRight (Css.px 5)
                , Css.display Css.inlineBlock
                , Css.verticalAlign Css.middle
                , Css.backgroundSize Css.cover
                , Css.height (Css.px 14)
                , Css.width (Css.px 14)
                , Css.backgroundImage (Css.url image)
                , Css.property "content" "''"
                ]

        trStat icon label stat =
            styled tr
                [ withStatIcon icon
                ]
                []
                [ styled td
                    [ Css.paddingRight (Css.px 18) ]
                    []
                    [ text label ]
                , td [] [ text stat ]
                ]
    in
    div []
        [ table []
            [ trStat shuffleIconData "Shuffle" <|
                if stats.isShuffle then
                    "Yes"

                else
                    "No"
            , trStat spellsPerCastIconData "Spells/Cast" <| fromInt stats.spellsPerCast
            , trStat castDelayIconData "Cast Delay" <| showTimeInteger stats.baseCastDelay
            , trStat rechargeTimeIconData "Recharge Time" <| showTimeInteger stats.rechargeTime
            , trStat manaMaxIconData "Mana Max" <| Round.round 0 stats.manaMax
            , trStat manaRechargeIconData "Mana chg spd" <| Round.round 0 stats.manaRechargeRate
            , trStat capacityIconData "Capacity" <| fromInt <| stats.wandCapacity - length wand.alwaysCast
            , trStat spreadIconData "Spread" <| Round.round 1 stats.spreadDegrees ++ " DEG"
            ]
        ]


viewSpellSlot : Maybe Spell -> Html msg
viewSpellSlot slot =
    let
        backgroundTile =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABQAAAAUCAYAAACNiR0NAAAAmklEQVQ4jWNgoDJghDEq3S3+w9hHXn6+bSPOq0osm4GBgaF95wlGuIGV7hb/j7z8fJsSl9mI86q27zzByMTAwMDw7OUXSsxiQDaDiWKT0AATAwMDwz2G/xR5F9kMJgYGiP8pNRBmBm3CcNTLZIFRLw92L//8/YdiA2FmwAvYCG0NeAH7lJX5tvTvv6rEshkYGBhWXL0BN4uqAAB3tmuIr9eIsgAAAABJRU5ErkJggg=="

        viewSpell spell =
            styled img
                [ Css.width (Css.px 30)
                , Css.transform <| Css.translate2 (Css.px 3) (Css.px 3)
                ]
                [ src <| "data:image/png;base64, " ++ spell.sprite ]
                []
    in
    styled div
        [ Css.width (Css.px 36)
        , Css.height (Css.px 36)
        , Css.position Css.relative
        , Css.backgroundSize Css.cover
        , Css.backgroundImage (Css.url backgroundTile)
        ]
        []
    <|
        case slot of
            Nothing ->
                []

            Just actualSpell ->
                [ viewSpell actualSpell
                , viewSpellTooltip actualSpell
                ]


viewSpellDeck : SpellData -> List SpellName -> Html msg
viewSpellDeck spellData spellNames =
    let
        spells =
            spellNames
                |> List.map (\name -> Dict.get name spellData)
                |> List.map viewSpellSlot
    in
    styled div
        [ Css.displayFlex
        , Css.flexDirection Css.row
        , Css.flexWrap Css.wrap
        ]
        []
        spells


viewWandAlwaysCast : SpellData -> Wand -> Html msg
viewWandAlwaysCast spellData wand =
    case wand.alwaysCast of
        [] ->
            text ""

        spells ->
            styled div
                [ Css.paddingBottom (Css.px 20) ]
                []
                [ p [] [ text "Always Cast:" ]
                , viewSpellDeck spellData spells
                ]


viewSpellTooltip : Spell -> Html msg
viewSpellTooltip spell =
    let
        spellTypeIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAALElEQVQImWP8////fwYcgImBgYGBkZGRERvN8B8KYGwUMWwScJpsnYz4XAsAZ7VPwX9hvpcAAAAASUVORK5CYII="

        manaDrainIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAABGdBTUEAAK/INwWK6QAAABl0RVh0U29mdHdhcmUAQWRvYmUgSW1hZ2VSZWFkeXHJZTwAAABKSURBVHjaYvj//z8DCPyHMZAAQAAxQsXgEoxAAGMDBBATAxYAMwUggLBKwgBAADEhG4VsJAgABBDYTmQxZIcBBBADFkfCFQAEGACg3Bz7y1UCwAAAAABJRU5ErkJggg=="

        castDelayIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAAFElEQVQImWNgoB34////f2w0jQAAvkUL9V/xo/8AAAAASUVORK5CYII="

        spreadIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAAH0lEQVR4nGNkQAX///9H56MIofOxaMGnFqscI1Y3AAD/ySDi7uHLHwAAAABJRU5ErkJggg=="

        damageFireIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAALUlEQVR4nG2NsQ0AMAzC7P7/M93SCJUNsAAASMLS2WY6C1SbHaLTz+7b6R8FLlHRDwJpYZNpAAAAAElFTkSuQmCC"

        damageMeleeIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAAL0lEQVR4nG2MQQ4AQAQDO/7/5zpIrFhzkNRQSbK9Zuik5ILaAlO/APQrfwkQfTVJ9y8a+X8G/0MAAAAASUVORK5CYII="

        damageProjectileIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAAL0lEQVR4nG2MQQ4AQAQDO/7/5zpIrFhzkNRQSbK9Zuik5ILaAlO/APQrfwkQfTVJ9y8a+X8G/0MAAAAASUVORK5CYII="

        damageElectricIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAAJElEQVR4nGNgQAL///+HMJgwhVBUIYsyYqpiZGTEqRyHEKYhAJehHem4dGeIAAAAAElFTkSuQmCC"

        damageExplosionIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAALklEQVR4nG2NyQkAQAwCx/6Lnn0EQsjGhweIogLANiWTs4pJgHSeuLv37ncN6gOsWSzdqkHLBAAAAABJRU5ErkJggg=="

        explosionRadiusIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAAK0lEQVR4nGNkYGBgYGD4//8/hMHIyMiAJoRgIwvBJZgYsAHsotjNZcTqBgDKwxf1KgQGRAAAAABJRU5ErkJggg=="

        speedMultiplierIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAAJUlEQVR4nGNgwAr+//+PyWbCKsGEVQdUlJGRES7KyMiI3VzsAADKfRf1EQt+BQAAAABJRU5ErkJggg=="

        maxUsesIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAAHElEQVR4nGNggIH///8zIANkProcToUkKsdnJwAxyxfpXRshNwAAAABJRU5ErkJggg=="

        bouncesIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAIAAABLMMCEAAAABnRSTlMAAAAAAABupgeRAAAAIUlEQVR4nGNgoAj8//+f4f///yh8NA6KEBZVDAxMWI0GALJMEfHRmBvkAAAAAElFTkSuQmCC"

        critChanceIconData =
            "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAcAAAAHCAYAAADEUlfTAAAAIklEQVQImWNggIL/////Z8AH/kMBjM+ETyFWDooJREmQBABufSvXnOV3wQAAAABJRU5ErkJggg=="

        withStatIcon image =
            Css.before
                [ Css.marginRight (Css.px 5)
                , Css.display Css.inlineBlock
                , Css.verticalAlign Css.middle
                , Css.backgroundSize Css.cover
                , Css.height (Css.px 14)
                , Css.width (Css.px 14)
                , Css.backgroundImage (Css.url image)
                , Css.property "content" "''"
                ]

        labelIconMapping label =
            case label of
                "Type" ->
                    spellTypeIconData

                "Mana drain" ->
                    manaDrainIconData

                "Cast delay" ->
                    castDelayIconData

                "Spread" ->
                    spreadIconData

                "Damage Fire" ->
                    damageFireIconData

                "Damage Melee" ->
                    damageMeleeIconData

                "Damage" ->
                    damageProjectileIconData

                "Damage Electric" ->
                    damageElectricIconData

                "Damage Explosion" ->
                    damageExplosionIconData

                "Explosion Radius" ->
                    explosionRadiusIconData

                "Speed Multiplier" ->
                    speedMultiplierIconData

                "Max Uses" ->
                    maxUsesIconData

                "Bounces" ->
                    bouncesIconData

                "Crit Chance" ->
                    critChanceIconData

                _ ->
                    ""

        trAttr label stat =
            styled tr
                [ withStatIcon <| labelIconMapping label
                ]
                []
                [ styled td
                    [ Css.paddingRight (Css.px 18) ]
                    []
                    [ text label ]
                , td [] [ text stat ]
                ]

        trSpace =
            styled tr
                [ Css.height (Css.px 14)
                ]
                []
                []

        opt v html =
            case v of
                Just j ->
                    html j

                Nothing ->
                    text ""

        viewSpellAttributes =
            table
                []
                [ trAttr "Type" <| spellTypeToString spell.meta.spellType
                , trAttr "Mana drain" <| fromInt spell.meta.manaDrain
                , trSpace
                , opt spell.meta.damageProjectile <| trAttr "Damage" << fromInt
                , opt spell.meta.damageSlice <| trAttr "Dmg. Slice" << fromInt
                , opt spell.meta.damageExplosion <| trAttr "Dmg. Expl" << fromInt
                , opt spell.meta.explosionRadius <| trAttr "Expl. Radius" << fromInt
                , opt spell.meta.damageFire <| trAttr "Dmg. Fire" << fromInt
                , opt spell.meta.damageDrill <| trAttr "Dmg. Drill" << fromInt
                , opt spell.meta.damageElectric <| trAttr "Dmg. Electric" << fromInt
                , opt spell.meta.bounces <| trAttr "Bounces" << fromInt
                , trSpace
                , opt spell.meta.castDelay <| trAttr "Cast delay" << showTimeInteger
                , opt spell.meta.rechargeDelay <| trAttr "Recharge delay" << showTimeInteger
                , opt spell.meta.spread <| trAttr "Spread" << (\f -> Round.round 0 f ++ " DEG")
                , opt spell.meta.critChance <| trAttr "Crit. Chance" << (\i -> "+" ++ fromInt i ++ "%")
                , case spell.meta.speedMultiplier of
                    Just x ->
                        if x /= 1.0 then
                            trAttr "Proj. Speed" <| "x " ++ Round.round 2 x

                        else
                            text ""

                    Nothing ->
                        text ""
                ]
    in
    styled div
        [ Css.padding (Css.px 5)
        , Css.border3 (Css.px 2) Css.solid (Css.rgb 255 255 255)
        , Css.borderRadius (Css.px 2)
        , Css.backgroundColor (Css.rgb 17 13 12)
        , Css.position Css.absolute
        , Css.width (Css.px 250)
        , Css.displayFlex
        , Css.zIndex (Css.int 1)
        , Css.flexDirection Css.column
        , Css.fontSize (Css.rem 0.8)
        ]
        [ class "displayOnParentHover" ]
        [ styled span [ Css.marginBottom (Css.rem 0.5) ] [] [ text spell.name ]
        , styled span [ Css.marginBottom (Css.rem 0.3) ] [] [ text spell.description ]
        , viewSpellAttributes
        ]
