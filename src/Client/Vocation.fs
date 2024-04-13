module Vocation

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.Skill

type Msg =
    | SetName of string
    | ZeroToFiveMsg of ZeroToFive.Msg
    | ToggleGoveringAttribute of AttributeName
    | VocationSkillListMsg of VocationSkillList.Msg
    | CalculateDicePools of DicePoolCalculationData

let init () : Vocation = {
    name = ""
    level = ZeroToFive.init ()
    governingAttributeNames = Set.empty
    dicePool = emptyDicePool
    vocationSkillList = VocationSkillList.init ()
}

let update msg (model: Vocation) =
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
    | VocationSkillListMsg msg -> {
        model with
            vocationSkillList = VocationSkillList.update msg model.vocationSkillList
      }
    | CalculateDicePools msg -> {
        model with
            dicePool = calculateVocationDicePool msg model.level model.governingAttributeNames
            vocationSkillList =
                VocationSkillList.update (VocationSkillList.CalculateDicePools(msg)) model.vocationSkillList
      }


open Feliz
open Feliz.Bulma

let view attributeNameSet (model: Vocation) dispatch =
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
                    model.governingAttributeNames
                    (ToggleGoveringAttribute >> dispatch)
                    attributeNameSet
            ]
            Bulma.column [ ZeroToFive.view model.level (ZeroToFiveMsg >> dispatch) ]
            Bulma.column [ prop.text (dicePoolToString model.dicePool) ]
        ]
        VocationSkillList.view attributeNameSet model.vocationSkillList (VocationSkillListMsg >> dispatch)
    ]