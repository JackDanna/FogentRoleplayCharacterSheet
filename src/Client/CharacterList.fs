module CharacterList

open Elmish

open Shared
open Fable.Remoting.Client

open FogentRoleplayLib.Character
open FogentRoleplayLib.SettingData

open Shared.UserData


type Model = {
    characterList: Character List
    selectedCharacter: int option
}

let fogentRoleplayDataApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IFogentRoleplayDataApi>

let init (userData: UserData) =
    {
        characterList = []
        selectedCharacter = None
    },
    Cmd.none

type Msg =
    | SelectCharacter of int
    | DeleteCharacter of int
    | AddNewCharacter
    | GotInitSettingData of SettingData
    | CharacterMsg of Character.Msg

let update msg model =
    match msg with
    | SelectCharacter pos ->
        let indexExists = List.tryItem pos model.characterList |> Option.isSome

        if indexExists then
            {
                model with
                    selectedCharacter = Some pos
            }
        else
            model
        |> (fun model -> model, Cmd.none)

    | DeleteCharacter pos ->
        {
            model with
                characterList = List.removeAt pos model.characterList
        },
        Cmd.none

    | AddNewCharacter -> model, Cmd.OfAsync.perform fogentRoleplayDataApi.getInitData () GotInitSettingData
    | GotInitSettingData settingData ->
        {
            model with
                characterList = List.append model.characterList [ Character.init (settingData) ]
        },
        Cmd.none

    | CharacterMsg msg ->
        match model.selectedCharacter with
        | None -> model, Cmd.none
        | Some pos ->
            let updatedCharacter = Character.update msg model.characterList[pos]

            {
                model with
                    characterList =
                        model.characterList
                        |> List.mapi (fun index x -> if index = pos then updatedCharacter else x)
            },
            Cmd.none

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.container [

        Bulma.container (
            List.mapi
                (fun position character ->
                    Bulma.container [
                        Html.text character.name
                        Html.button [
                            prop.onClick (fun _ -> dispatch (SelectCharacter position))
                            prop.text "Select"
                        ]
                    ])
                model.characterList
            |> List.append [
                Html.button [
                    prop.onClick (fun _ -> (dispatch AddNewCharacter))
                    prop.text "Add New Character"
                ]
            ]
        )

        match model.selectedCharacter with
        | Some index -> Character.view model.characterList[index] (CharacterMsg >> dispatch)
        | None -> Html.none
    ]