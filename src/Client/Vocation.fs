module Vocation

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.DicePoolCalculation

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
    dicePoolModList = []
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
    | VocationSkillListMsg msg ->
        let newVocationSkillList = VocationSkillList.update msg model.vocationSkillList

        {
            model with
                vocationSkillList =
                    match msg with
                    | VocationSkillList.ModifiedVocationSkillAtPosition(position, _) ->
                        newVocationSkillList
                        |> VocationSkillList.update (VocationSkillList.CheckIfLevelCapExceeded(position, model.level))
                    | _ -> newVocationSkillList
        }
    | CalculateDicePools msg -> {
        model with
            dicePoolModList = calculateVocationDicePool msg model.level model.governingAttributeNames
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
            Bulma.column [ prop.text (model.dicePoolModList |> dicePoolModListToString) ]
        ]
        VocationSkillList.view attributeNameSet model.vocationSkillList (VocationSkillListMsg >> dispatch)
    ]