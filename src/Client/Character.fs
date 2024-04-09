module Character

open FogentRoleplayLib.Character
open FogentRoleplayLib.Attribute
open FogentRoleplayLib.CoreSkill

type Msg =
    | SetName of string
    | AttributeAndCoreSkillsListMsg of AttributeAndCoreSkillsList.Msg

let init (attributeData: Attribute list) (coreSkillData: CoreSkill list) = {
    name = ""
    attributeAndCoreSkillsList = defaultAttributeAndCoreSkillsList attributeData coreSkillData
}

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