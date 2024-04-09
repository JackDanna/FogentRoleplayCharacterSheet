module Index

open Elmish

open Fable.Remoting.Client
open Shared

open FogentRoleplayLib.Character

type Model = {
    fogentRoleplayData: FogentRoleplayData
    character: Character
}

type Msg =
    | CharacterMsg of Character.Msg
    | GotInitData of FogentRoleplayData

let fogentRoleplayDataApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IFogentRoleplayDataApi>

let init () : Model * Cmd<Msg> =
    let defaultAttributeList = []
    let defaultCoreSkillList = []

    {
        fogentRoleplayData = {
            defaultAttributeList = defaultAttributeList
            defaultCoreSkillList = defaultCoreSkillList
        }
        character = Character.init defaultAttributeList defaultCoreSkillList
    },

    Cmd.OfAsync.perform fogentRoleplayDataApi.getInitData () GotInitData

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | CharacterMsg characterMsg ->

        {
            model with
                character = Character.update characterMsg model.character
        },
        Cmd.none
    | GotInitData newFallenData ->

        {
            model with
                fogentRoleplayData = newFallenData
                character = Character.init newFallenData.defaultAttributeList newFallenData.defaultCoreSkillList
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
                    // (Seq.toList model.fallenData.carryWeightCalculationMap.Keys)
                    // ((List.ofSeq model.fallenData.effectForDisplayMap.Keys)
                    //  @ (List.ofSeq model.fallenData.carryWeightCalculationMap.Keys)
                    //  @ (List.ofSeq model.fallenData.movementSpeedCalculationMap.Keys))
                    // model.fallenData.combatVocationalSkill
                    // model.fallenData.allItemStackList
                    model.character
                    (CharacterMsg >> dispatch)
            ]
        ]
    ]