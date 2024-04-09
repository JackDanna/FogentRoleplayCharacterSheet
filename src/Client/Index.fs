module Index

open Elmish

open Fable.Remoting.Client
open Shared

open FogentRoleplayLib.Character

type Model = {
    fallenData: FogentRoleplayData
    character: Character
}

type Msg =
    | CharacterMsg of Character.Msg
    | GotInitData of FogentRoleplayData

let fallenDataApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IFogentRoleplayDataApi>

let init () : Model * Cmd<Msg> =
    {
        fallenData = {
            defaultCoreSkillList = []
            defaultAttributeList = []
        //   allItemStackList = []
        //   magicSkillMap = Map.empty
        //   magicCombatMap = Map.empty
        //   rangeMap = Map.empty
        //   combatVocationalSkill = []
        //   effectForDisplayMap = Map.empty
        //   carryWeightCalculationMap = Map.empty
        //   weightClassList = []
        //   movementSpeedCalculationMap = Map.empty
        }
        character = Character.init
    },

    Cmd.OfAsync.perform fallenDataApi.getInitData () GotInitData

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | CharacterMsg characterMsg ->

        {
            model with
                character =
                    Character.update
                        // model.fallenData.defaultAttributeList
                        // model.fallenData.defaultCoreSkillList
                        // model.fallenData.allItemStackList
                        // model.fallenData.magicSkillMap
                        // model.fallenData.magicCombatMap
                        // model.fallenData.rangeMap
                        // model.fallenData.effectForDisplayMap
                        // model.fallenData.carryWeightCalculationMap
                        // model.fallenData.weightClassList
                        // model.fallenData.movementSpeedCalculationMap
                        characterMsg
                        model.character
        },
        Cmd.none
    | GotInitData newFallenData ->

        {
            model with
                fallenData = newFallenData
                character = Character.update Character.Msg.SetDefault model.character
        },
        Cmd.none

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isDanger

        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl
                "https://www.onlygfx.com/wp-content/uploads/2015/12/simple-old-paper-1-transparent.jpg"
            style.backgroundPosition "no-repeat center center fixed"
        ]

        prop.children [

            Bulma.heroHead [
                Bulma.navbar [
                    color.isPrimary
                    prop.children [
                        Bulma.navbarItem.div [
                            Bulma.title.h3 [ prop.text "Fallen"; prop.style [ style.fontFamily "PT Serif Caption" ] ]
                        ]
                    ]
                ]
            ]

            Bulma.heroBody [
                Character.view
                    (Seq.toList model.fallenData.carryWeightCalculationMap.Keys)
                    ((List.ofSeq model.fallenData.effectForDisplayMap.Keys)
                     @ (List.ofSeq model.fallenData.carryWeightCalculationMap.Keys)
                     @ (List.ofSeq model.fallenData.movementSpeedCalculationMap.Keys))
                    model.fallenData.combatVocationalSkill
                    model.fallenData.allItemStackList
                    model.character
                    (CharacterMsg >> dispatch)
            ]
        ]
    ]