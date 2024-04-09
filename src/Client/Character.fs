module Character

open FogentRoleplayLib.Character

type Msg =
    | SetName of string
    | AttributeAndCoreSkillsListMsg of AttributeAndCoreSkillsList.Msg

let update msg (model: Character) =
    match msg with
    | SetName newName -> { model with name = newName }
    | AttributeAndCoreSkillsListMsg msg -> {
        model with
            attributeAndCoreSkillsList = AttributeAndCoreSkillsList.update msg model.attributeAndCoreSkillsList
      }

open Feliz
open Feliz.Bulma

let view model dispatch = Html.none