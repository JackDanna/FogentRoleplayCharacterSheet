module Skill

open FogentRoleplayLib.Skill
open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.BaseDiceEffect

type Msg =
    | Neg1To5Msg of Neg1To5.Msg
    | SetEffectDicePoolModList of DicePoolMod List

let init name (baseDiceEffectList: BaseDiceEffect List) effectDicePoolModList =

    {
        name = name
        level = Neg1To5.init ()
        baseDice = findBaseDiceForSkill baseDiceEffectList name
        effectDicePoolModList = effectDicePoolModList
    }

let update msg model =
    match msg with
    | Neg1To5Msg msg -> {
        model with
            level = Neg1To5.update msg model.level
      }
    | SetEffectDicePoolModList effectDicePoolModList -> {
        model with
            effectDicePoolModList = effectDicePoolModList
      }


open Feliz
open Feliz.Bulma

let view (model: Skill) dispatch disableChangeLevel governingSkillColumn =
    [ Bulma.column [ prop.text model.name ] ]
    @ match governingSkillColumn with
      | Some column -> column |> List.singleton
      | None -> List.Empty
    @ [
        Bulma.column [ Neg1To5.view model.level (Neg1To5Msg >> dispatch) disableChangeLevel ]
        Bulma.column [ skillToDicePoolString model |> prop.text ]
    ]