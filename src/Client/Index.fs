module Index

open Elmish

open Fable.Remoting.Client
open Shared

open FogentRoleplayLib.Character
open FogentRoleplayLib.SettingData

type Model = { character: Character }

type Msg =
    | CharacterMsg of Character.Msg
    | GotInitSettingData of SettingData

let fogentRoleplayDataApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IFogentRoleplayDataApi>

let init () : Model * Cmd<Msg> =
    let defaultAttributeSet = Set.empty
    let defaultCoreSkillList = Set.empty

    {
        character = Character.init Set.empty (FogentRoleplayLib.SettingData.init ())
    },

    Cmd.OfAsync.perform fogentRoleplayDataApi.getInitData () GotInitSettingData

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | CharacterMsg characterMsg ->


        match characterMsg with

        | Character.EffectListMsg(msg, _) ->
            match msg with
            | EffectList.Insert(effectName, _) ->
                (EffectList.Insert(effectName, Some model.fogentRoleplayData.effectMap))
            | _ -> msg
            |> (fun msg -> Character.EffectListMsg(msg, Some model.fogentRoleplayData.weaponSkillDataMap))
            |> (fun msg ->
                {
                    model with
                        character = Character.update msg model.character
                },
                Cmd.none)

        | Character.CombatSpeedsMsg(CombatSpeeds.Insert(name, x, y, _)) ->
            {
                model with
                    character =
                        Character.update
                            (Character.CombatSpeedsMsg(
                                CombatSpeeds.Insert(name, x, y, Some model.fogentRoleplayData.combatSpeedCalculationMap)
                            ))
                            model.character
            },
            Cmd.none

        | Character.EquipmentMsg(msg, _) ->
            match msg with
            | ItemStackList.Insert(msg, _) -> ItemStackList.Insert(msg, Some model.fogentRoleplayData.itemStackMap)
            | _ -> msg
            |> (fun msg -> Character.EquipmentMsg(msg, Some model.fogentRoleplayData.weaponSkillDataMap))
            |> (fun msg -> {
                model with
                    character = Character.update msg model.character
            }),
            Cmd.none

        | _ ->
            {
                model with
                    character = Character.update characterMsg model.character
            },
            Cmd.none
    | GotInitSettingData newFogentRoleplayData ->

        {
            model with
                fogentRoleplayData = newFogentRoleplayData
                character = Character.init newFogentRoleplayData.coreSkillDataSet
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

            Bulma.heroBody [ Character.view model.character (CharacterMsg >> dispatch) ]
        ]
    ]