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
    | SettingListMsg of int * Setting.Msg

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
        Cmd.map (fun settingMsg -> SettingListMsg(settingModel.id, settingMsg)) settingCmd

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
                selectedSetting = Some settingId
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

    | SettingListMsg(settingId, settingMsg) ->
        settingId
        |> tryFindSetting model
        |> Option.map (Setting.update addNewCharacterApi settingMsg)
        |> handleUpdatedSetting model

open Feliz
open Feliz.DaisyUI

let view (model: Model) dispatch =
    Daisy.drawer [
        prop.className "lg:drawer-open rounded-lg shadow bg-base-200 h-screen"
        prop.children [
            Daisy.drawerToggle [ prop.id "my-drawer" ]
            Daisy.drawerContent [
                prop.className "flex flex-col items-center justify-center"
                prop.children [
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
            Daisy.drawerSide [
                prop.className "absolute h-full"
                prop.children [
                    Daisy.drawerOverlay [ prop.htmlFor "my-drawer" ]
                    Daisy.menu [
                        prop.className "p-4 h-full overflow-y-auto w-80 bg-base-100 text-base-content"
                        prop.children (
                            model.settings
                            |> Seq.map (fun setting ->
                                Setting.view
                                    setting
                                    ((fun msg -> SettingListMsg(setting.id, msg)) >> dispatch)
                                    (fun settingId characterId ->
                                        dispatch (SelectSettingAndCharacter(settingId, characterId))))
                        )
                    ]

                ]
            ]

        ]
    ]