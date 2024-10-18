[<RequireQualifiedAccess>]
module Overview

open Elmish

open Fable.Remoting.Client
open Shared
open FogentRoleplayLib.Character
open FogentRoleplayLib.Setting


type Model = {
    User: Shared.UserData
    settings: Setting seq
    selectedSetting: int option
    selectedCharacter: int option
}

let getUserApi token =
    Remoting.createApi ()
    |> Remoting.withAuthorizationHeader $"Bearer {token}"
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IUserApi>

type Msg =
    | GotSettings of Setting seq

    | SelectSettingAndCharacter of int * int
    | DeleteCharacter of int

    | CharacterMsg of Character.Msg
    | SettingMsg of Setting.Msg

let init (user: UserData) =

    {
        User = user
        settings = Seq.empty
        selectedSetting = None
        selectedCharacter = None
    },
    Cmd.OfAsync.perform (getUserApi user.token).getOwnedSettingApi user.username GotSettings

let tryFindSetting model settingId =
    model.settings |> Seq.tryFind (fun setting -> setting.id = settingId)

let tryFindCharacterInSetting model settingId characterId =

    tryFindSetting model settingId
    |> Option.map (fun setting -> setting.characters)
    |> Option.bind (Seq.tryFind (fun character -> character.id = characterId))

let handleUpdatedSetting model =
    function
    | None -> model, Cmd.none
    | Some(settingModel, settingCmd) ->
        {
            model with
                settings =
                    Seq.map
                        (fun setting ->
                            if setting.id = settingModel.id then
                                settingModel
                            else
                                setting)
                        model.settings
        },
        Cmd.map SettingMsg settingCmd

let temp model =
    match model.selectedSetting, model.selectedCharacter with
    | Some settingId, Some characterId -> Some(settingId, characterId)
    | _, _ -> None

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =

    let userApi = getUserApi model.User.token
    let addNewCharacterApi = (userApi.addNewCharacterApi model.User.username)

    match msg with
    | GotSettings settings -> { model with settings = settings }, Cmd.none
    | SelectSettingAndCharacter(settingId, characterId) ->
        tryFindCharacterInSetting model settingId characterId
        |> Option.map (fun character -> {
            model with
                selectedCharacter = Some character.id
        })
        |> Option.defaultValue model,
        Cmd.none

    | DeleteCharacter id ->
        {
            model with
                settings = Seq.filter (fun idCharacter -> idCharacter.id <> id) model.settings
        },
        // Need to add deletion in the database
        Cmd.none


    | CharacterMsg characterMsg ->

        temp model
        |> Option.bind (fun (settingId, characterId) ->

            tryFindSetting model settingId
            |> Option.map (fun setting ->
                let preloadedAPI = userApi.updateCharacterApi model.User.username setting.id

                Setting.update
                    addNewCharacterApi
                    (Setting.CharacterListMsg(characterMsg, characterId, Some preloadedAPI))
                    setting))
        |> handleUpdatedSetting model

    | SettingMsg settingMsg ->
        model.selectedSetting
        |> Option.bind (fun selectedSettingId -> tryFindSetting model selectedSettingId)
        |> Option.map (Setting.update addNewCharacterApi settingMsg)
        |> handleUpdatedSetting model

open Feliz
open Feliz.Bulma

let view (model: Model) dispatch =
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

                Bulma.container [
                    model.settings
                    |> Seq.map (fun setting ->
                        Setting.view setting (SettingMsg >> dispatch) (fun settingId characterId ->
                            dispatch (SelectSettingAndCharacter(settingId, characterId))))
                    |> Bulma.container

                    model
                    |> temp
                    |> Option.bind (fun (selectedSettingId, selectedCharacterId) ->
                        Option.map2
                            (fun setting character -> (setting.SettingData, character))
                            (tryFindSetting model selectedSettingId)
                            (tryFindCharacterInSetting model selectedSettingId selectedCharacterId))
                    |> function
                        | None -> Html.none
                        | Some(settingData, character) ->
                            Character.view character (CharacterMsg >> dispatch) settingData
                ]
            ]
        ]
    ]