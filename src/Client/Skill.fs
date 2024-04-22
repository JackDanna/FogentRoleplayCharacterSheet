module Skill

open FogentRoleplayLib.Skill
open FogentRoleplayLib.DicePoolMod

type Msg =
    | SetName of string
    | Neg1To5Msg of Neg1To5.Msg
    | SetDicePoolModList of DicePoolMod List

let init () = {
    name = ""
    level = Neg1To5.init ()
    dicePoolModList = base3d6DicePoolMod |> List.singleton
}

let update msg model =
    match msg with
    | SetName msg -> { model with name = msg }
    | Neg1To5Msg msg -> {
        model with
            level = Neg1To5.update msg model.level
      }
    | SetDicePoolModList msg -> { model with dicePoolModList = msg }


open Feliz
open Feliz.Bulma

let view model dispatch canUserChangeName canUserChangeLevel governingSkillColumn =
    if canUserChangeName then
        [
            Bulma.column [
                Bulma.input.text [
                    prop.value model.name
                    prop.onTextChange (fun value -> dispatch (SetName value))
                ]
            ]
        ]
    else
        [ Bulma.column [ prop.text model.name ] ]
    @ match governingSkillColumn with
      | Some column -> column |> List.singleton
      | None -> List.Empty
    @ [
        Bulma.column [ Neg1To5.view model.level (Neg1To5Msg >> dispatch) canUserChangeLevel ]
        Bulma.column [ model.dicePoolModList |> dicePoolModListToString |> prop.text ]
    ]
    |> Bulma.columns
    |> Bulma.content