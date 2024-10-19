module Setting

open FogentRoleplayLib
open Character
open SettingData
open Setting

open Elmish

type Msg =
    | SetSettingData of SettingData
    | AddNewCharacter
    | AddedNewCharacter of Result<Character, string>
    | CharacterListMsg of Character.Msg * int * Option<Character -> Async<Result<unit, string>>>
    | UpdatedCharacter of Result<unit, string>

let update userApi (msg: Msg) (model: Setting) =

    match msg with
    | SetSettingData newSettingData ->
        {
            model with
                SettingData = newSettingData
        },
        Cmd.none

    | AddNewCharacter -> model, Cmd.OfAsync.perform userApi model.id AddedNewCharacter

    | AddedNewCharacter result ->
        match result with
        | Ok character ->
            {
                model with
                    characters = character |> Seq.singleton |> Seq.append model.characters
            },
            Cmd.none
        | Error _ -> model, Cmd.none

    | CharacterListMsg(msg, characterId, updateCharacterApiOption) ->
        model.characters
        |> Seq.tryFind (fun x -> x.id = characterId)
        |> function
            | None -> model, Cmd.none
            | Some character ->
                let updatedCharacter = Character.update msg character model.SettingData

                {
                    model with
                        characters =
                            Seq.map
                                (fun (x: Character) -> if x.id = updatedCharacter.id then updatedCharacter else x)
                                model.characters
                },
                match updateCharacterApiOption with
                | Some updateCharacterApi -> Cmd.OfAsync.perform updateCharacterApi updatedCharacter UpdatedCharacter
                | None -> Cmd.none
    | UpdatedCharacter result ->
        // Might wanna do something with this result at some point
        model, Cmd.none

open Feliz
open Feliz.Bulma

let view (model: Setting) dispatch selectSettingAndCharacter =
    Bulma.container [
        Html.text model.name
        Bulma.container (

            Seq.append
                (Seq.map
                    (fun (character: Character) ->
                        Bulma.container [
                            Html.text character.name
                            Html.button [
                                prop.onClick (fun _ -> selectSettingAndCharacter model.id character.id)
                                prop.text "Select"
                            ]
                        ])
                    model.characters)

                [
                    Html.button [
                        prop.onClick (fun _ -> dispatch AddNewCharacter)
                        prop.text "Add New Character"
                    ]
                ]
        )
    ]