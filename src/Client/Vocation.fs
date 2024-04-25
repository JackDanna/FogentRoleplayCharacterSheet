module Vocation

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.Character

type Msg =
    | SetName of string
    | ZeroToFiveMsg of ZeroToFive.Msg
    | ToggleGoveringAttribute of AttributeName
    | VocationSkillListMsg of VocationSkillList.Msg
    | CalculateDicePools of DicePoolCalculationData

let init () : Vocation = {
    name = ""
    level = ZeroToFive.init ()
    governingAttributeNameSet = Set.empty
    baseDice = base3d6DicePool
    dicePoolModList = []
    vocationSkillList = VocationSkillList.init ()
}

let update msg (model: Vocation) =
    match msg with
    | SetName newName -> { model with name = newName }
    | ZeroToFiveMsg msg ->
        let newLevel = ZeroToFive.update msg model.level

        {
            model with
                level = newLevel
                vocationSkillList =
                    VocationSkillList.update
                        (VocationSkillList.Msg.CheckIfLevelCapExeededForAll newLevel)
                        model.vocationSkillList
        }
    | ToggleGoveringAttribute newAttributeName -> {
        model with
            governingAttributeNameSet = toggleAttributeNameSet model.governingAttributeNameSet newAttributeName
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
            dicePoolModList = calculateVocationDicePoolModList msg model.level model.governingAttributeNameSet
            vocationSkillList =
                VocationSkillList.update (VocationSkillList.CalculateDicePools(msg)) model.vocationSkillList
      }


open Feliz
open Feliz.Bulma

let view attributeNameSet (vocationSkillData: VocationSkillData) (model: Vocation) dispatch =
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
                    model.governingAttributeNameSet
                    (ToggleGoveringAttribute >> dispatch)
                    attributeNameSet
            ]
            Bulma.column [ ZeroToFive.view model.level (ZeroToFiveMsg >> dispatch) ]
            Bulma.column [
                prop.text (
                    modifyDicePoolByDicePoolModList model.baseDice model.dicePoolModList
                    |> dicePoolToString
                )
            ]
        ]
        VocationSkillList.view
            attributeNameSet
            (if vocationSkillData.magicSystemMap.ContainsKey model.name then
                 let magicSystem = vocationSkillData.magicSystemMap.Item model.name
                 magicSystem.magicSkillDataSet
             else
                 Set.empty)
            vocationSkillData.weaponGoverningSkillNameSet
            model.vocationSkillList
            (VocationSkillListMsg >> dispatch)
    ]