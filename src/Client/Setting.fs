module Setting

open FogentRoleplayLib
open Character
open SettingData
open Setting

open Elmish

type Msg =
    | SetSettingData of SettingData
    | InsertNewCharacter of Character
    | CharacterListMsg of Character.Msg * int * Option<Character -> Async<Result<unit, string>>>
    | UpdatedCharacter of Result<unit, string>

let update (msg: Msg) (model: Setting) =

    match msg with
    | SetSettingData newSettingData ->
        {
            model with
                SettingData = newSettingData
        },
        Cmd.none
    | InsertNewCharacter character ->
        {
            model with
                characters = Seq.append model.characters (Seq.singleton character)
        },
        Cmd.none
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