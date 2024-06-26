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
        character = Character.init (FogentRoleplayLib.SettingData.init ())
    },

    Cmd.OfAsync.perform fogentRoleplayDataApi.getInitData () GotInitSettingData

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | CharacterMsg characterMsg ->

        {
            model with
                character = Character.update characterMsg model.character
        },
        Cmd.none
    | GotInitSettingData newSettingData ->

        {
            model with
                character = Character.init newSettingData
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