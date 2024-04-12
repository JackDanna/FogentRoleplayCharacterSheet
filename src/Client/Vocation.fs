module Vocation

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.DicePool

type Msg =
    | SetName of string
    | ZeroToFiveMsg of ZeroToFive.Msg
    | ToggleGoveringAttribute of AttributeName

let init () : Vocation = {
    name = ""
    level = ZeroToFive.init ()
    governingAttributeNames = Set.empty
    dicePool = emptyDicePool
    vocationSkillList = VocationSkillList.init ()
}

let update msg model =
    match msg with
    | SetName newName -> { model with name = newName }
    | ZeroToFiveMsg msg -> {
        model with
            level = ZeroToFive.update msg model.level
      }
    | ToggleGoveringAttribute newAttributeName -> {
        model with
            governingAttributeNames = toggleAttributeNameSet model.governingAttributeNames newAttributeName
      }

open Feliz
open Feliz.Bulma

let view attributeNameSet model dispatch =
    Bulma.box [
        Bulma.columns [
            Bulma.column [
                Bulma.input.text [
                    prop.value model.name
                    prop.onTextChange (fun value -> dispatch (SetName value))
                ]
            ]
            Bulma.column [
                VocationalSkill.governingAttributesToggle
                    attributeNameSet
                    (ToggleGoveringAttribute >> dispatch)
                    model.governingAttributeNames
            ]
            Bulma.column [ ZeroToFive.view model.level (ZeroToFiveMsg >> dispatch) ]
            Bulma.column [ prop.text (dicePoolToString model.dicePool) ]
        ]
    ]