module CharacterList

open Elmish

open Shared

open Fable.Remoting.Client

open FogentRoleplayLib.Character
open FogentRoleplayLib.SettingData


type Model = {
    characterList: Character List
    selectedCharacter: int option
}

let getUserApi (token: JWT) =
    Remoting.createApi ()
    |> Remoting.withAuthorizationHeader $"Bearer {token}"
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IUserApi>

type Msg =
    | GotCharacterList of Character list
    | SelectCharacter of int
    | DeleteCharacter of int
    | AddNewCharacter of SettingData option
    | CharacterMsg of Character.Msg

let init (userData: UserData) =

    let api = getUserApi userData.token

    let getCharacterList = async {
        let! result = api.getCharacterList userData
        return GotCharacterList(result)
    }

    {
        characterList = []
        selectedCharacter = None
    },
    Cmd.fromAsync getCharacterList

let update msg model =
    match msg with
    | GotCharacterList characterList -> {
        model with
            characterList = characterList
      }
    | SelectCharacter pos ->
        if List.tryItem pos model.characterList |> Option.isSome then
            {
                model with
                    selectedCharacter = Some pos
            }
        else
            model

    | DeleteCharacter pos -> {
        model with
            characterList = List.removeAt pos model.characterList
      }

    | AddNewCharacter settingDataOption ->
        match settingDataOption with
        | None -> model
        | Some settingData -> {
            model with
                characterList = List.append model.characterList [ Character.init (settingData) ]
          }

    | CharacterMsg msg ->
        match model.selectedCharacter with
        | None -> model
        | Some pos ->
            let updatedCharacter = Character.update msg model.characterList[pos]

            {
                model with
                    characterList =
                        model.characterList
                        |> List.mapi (fun index x -> if index = pos then updatedCharacter else x)
            }

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
                    prop.onClick (fun _ -> (dispatch (AddNewCharacter None)))
                    prop.text "Add New Character"
                ]
            ]
        )

        match model.selectedCharacter with
        | Some index -> Character.view model.characterList[index] (CharacterMsg >> dispatch)
        | None -> Html.none
    ]