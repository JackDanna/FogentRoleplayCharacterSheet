module Skill

open FogentRoleplayLib.Skill
open FogentRoleplayLib.DicePool

type Msg =
    | Neg1To5Msg of Neg1To5.Msg
    | SetDicePool of DicePool

let init () = {
    name = ""
    level = Neg1To5.init ()
    dicePool = baseDicePool
}

let update msg model =
    match msg with
    | Neg1To5Msg msg -> {
        model with
            level = Neg1To5.update msg model.level
      }
    | SetDicePool msg -> { model with dicePool = msg }

open Feliz
open Feliz.Bulma

let view model dispatch canUserChangeLevel governingSkillColumn =

    [ Bulma.column [ prop.text model.name ] ]
    @ match governingSkillColumn with
      | Some column -> column |> List.singleton
      | None -> List.Empty
    @ [
        Bulma.column [ Neg1To5.view model.level (Neg1To5Msg >> dispatch) canUserChangeLevel ]
        Bulma.column [ model.dicePool |> dicePoolToString |> prop.text ]
    ]
    |> Bulma.columns
    |> Bulma.content