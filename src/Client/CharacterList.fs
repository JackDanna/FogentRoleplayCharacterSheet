module CharacterList

open Elmish

open Shared

open Fable.Remoting.Client

open FogentRoleplayLib.Character
open FogentRoleplayLib.SettingData


type Model = {
    idCharacterList: IdCharacter List
    selectedCharacter: int option
}

let getUserApi (token: JWT) =
    Remoting.createApi ()
    |> Remoting.withAuthorizationHeader $"Bearer {token}"
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IUserApi>

type Msg =
    | GotIdCharacterList of IdCharacter list
    | SelectCharacter of int
    | DeleteCharacter of int
    | AddNewCharacter of SettingData option
    | CharacterMsg of Character.Msg

let init (userData: UserData) =

    let api = getUserApi userData.token

    let getCharacterList = async {
        let! result = api.getIdCharacterList userData
        return GotIdCharacterList(result)
    }

    {
        idCharacterList = []
        selectedCharacter = None
    },
    Cmd.fromAsync getCharacterList

let update msg model =
    match msg with
    | GotIdCharacterList idCharacterList -> {
        model with
            idCharacterList = idCharacterList
      }
    | SelectCharacter pos ->
        if List.tryItem pos model.idCharacterList |> Option.isSome then
            {
                model with
                    selectedCharacter = Some pos
            }
        else
            model

    | DeleteCharacter pos -> {
        model with
            idCharacterList = List.removeAt pos model.idCharacterList
      }

    | AddNewCharacter settingDataOption ->
        match settingDataOption with
        | None -> model
        | Some settingData -> {
            model with
                idCharacterList =
                    settingData
                    |> Character.init
                    |> createAutoIncrementedIdCharacter
                    |> List.singleton
                    |> List.append model.idCharacterList
          }

    | CharacterMsg msg ->
        match model.selectedCharacter with
        | None -> model
        | Some pos ->

            {
                model with
                    idCharacterList =
                        model.idCharacterList
                        |> List.mapi (fun index x ->
                            if index = pos then
                                {
                                    model.idCharacterList[pos] with
                                        Character = Character.update msg model.idCharacterList[pos].Character
                                }
                            else
                                x)
            }

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.container [

        Bulma.container (
            List.mapi
                (fun position character ->
                    Bulma.container [
                        Html.text character.Character.name
                        Html.button [
                            prop.onClick (fun _ -> dispatch (SelectCharacter position))
                            prop.text "Select"
                        ]
                    ])
                model.idCharacterList
            |> List.append [
                Html.button [
                    prop.onClick (fun _ -> (dispatch (AddNewCharacter None)))
                    prop.text "Add New Character"
                ]
            ]
        )

        match model.selectedCharacter with
        | Some index -> Character.view model.idCharacterList[index].Character (CharacterMsg >> dispatch)
        | None -> Html.none
    ]