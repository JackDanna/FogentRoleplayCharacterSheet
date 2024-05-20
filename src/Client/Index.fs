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
    let defaultAttributeSet = Set.empty
    let defaultCoreSkillList = Set.empty

    {
        character = Character.init Set.empty
        fogentRoleplayData = {
            attributeNameSet = defaultAttributeSet
            attributeAndCoreSkillDataSet = defaultCoreSkillList
            itemStackMap = Map.empty
            weaponSpellSet = Set.empty
            magicSystemMap = Map.empty
            weaponSkillData = Set.empty
        }
    },

    Cmd.OfAsync.perform fogentRoleplayDataApi.getInitData () GotInitData

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | CharacterMsg characterMsg ->
        match characterMsg with
        | Character.VocationListMsg(VocationList.InsertVocation(x, y, z, _)) ->
            {
                model with
                    character =
                        Character.update
                            (Character.VocationListMsg(
                                VocationList.InsertVocation(x, y, z, Some model.fogentRoleplayData.magicSystemMap)
                            ))
                            model.character
            },
            Cmd.none
        | _ ->
            {
                model with
                    character = Character.update characterMsg model.character
            },
            Cmd.none
    | GotInitData newFogentRoleplayData ->

        {
            model with
                fogentRoleplayData = newFogentRoleplayData
                character = Character.init newFogentRoleplayData.attributeAndCoreSkillDataSet
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
                    model.fogentRoleplayData.attributeNameSet
                    model.fogentRoleplayData.itemStackMap
                    (model.fogentRoleplayData.magicSystemMap.Keys |> Set.ofSeq)
                    (model.fogentRoleplayData.weaponSkillData |> Set.map (_.name))
                    model.character
                    (CharacterMsg >> dispatch)
            ]
        ]
    ]