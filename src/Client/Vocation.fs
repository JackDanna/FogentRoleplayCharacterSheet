module Vocation

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.Character

open FogentRoleplayLib.MagicSystem

type Msg =
    | VocationStatMsg of VocationStat.Msg
    | VocationSkillListMsg of VocationSkillList.Msg
    | CalculateDicePools of DicePoolCalculationData

let init () : Vocation = {
    vocationStat = VocationStat.init ()
    vocationSkillList = VocationSkillList.init ()
}

let update msg (model: Vocation) =
    match msg with
    // | SetName newName -> { model with name = newName }
    // | SetMagicVocation (name, newAttributeNameSet) ->
    //     {
    //         model with
    //             name =
    //     }
    // | ZeroToFiveMsg msg ->
    //     let newLevel = ZeroToFive.update msg model.level

    //     {
    //         model with
    //             level = newLevel
    //             vocationSkillList =
    //                 VocationSkillList.update
    //                     (VocationSkillList.Msg.CheckIfLevelCapExeededForAll newLevel)
    //                     model.vocationSkillList
    //     }
    // | ToggleGoveringAttribute newAttributeName -> {
    //     model with
    //         governingAttributeNameSet = toggleAttributeNameSet model.governingAttributeNameSet newAttributeName
    //   }
    | VocationStatMsg msg ->
        let newVocationStat = VocationStat.update msg model.vocationStat

        match msg with
        | VocationStat.ZeroToFiveMsg _ -> {
            model with
                vocationStat = newVocationStat
                vocationSkillList =
                    VocationSkillList.update
                        (VocationSkillList.CheckIfLevelCapExeededForAll newVocationStat.level)
                        model.vocationSkillList
          }
        | _ -> {
            model with
                vocationStat = newVocationStat
          }

    | VocationSkillListMsg msg ->
        let newVocationSkillList = VocationSkillList.update msg model.vocationSkillList

        {
            model with
                vocationSkillList =
                    match msg with
                    | VocationSkillList.ModifiedVocationSkillAtPosition(position, _) ->
                        VocationSkillList.update
                            (VocationSkillList.CheckIfLevelCapExceeded(position, model.vocationStat.level))
                            newVocationSkillList

                    | _ -> newVocationSkillList
        }
    | CalculateDicePools msg -> {
        model with
            vocationStat = VocationStat.update (VocationStat.CalculateDicePool msg) model.vocationStat
            vocationSkillList =
                VocationSkillList.update (VocationSkillList.CalculateDicePools msg) model.vocationSkillList
      }


open Feliz
open Feliz.Bulma

let view attributeNameSet (vocationSkillData: VocationSkillData) (model: Vocation) dispatch =
    Bulma.box [
        VocationStat.view attributeNameSet model.vocationStat (VocationStatMsg >> dispatch)
        VocationSkillList.view
            attributeNameSet
            (if vocationSkillData.magicSystemMap.ContainsKey model.vocationStat.name then
                 let magicSystem = vocationSkillData.magicSystemMap.Item model.vocationStat.name
                 magicSystem.magicSkillDataSet
             else
                 Set.empty)
            vocationSkillData.weaponGoverningSkillNameSet
            model.vocationSkillList
            (VocationSkillListMsg >> dispatch)
    ]