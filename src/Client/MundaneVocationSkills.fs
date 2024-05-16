module MundaneVocationSkills

open FogentRoleplayLib.MundaneVocationSkills
open FogentRoleplayLib.ZeroToFive
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.VocationalSkill
open FogentRoleplayLib.WeaponSkillData

type Msg =
    | VocationalSkillListMsg of VocationalSkillList.Msg
    | WeaponSkillListMsg of WeaponSkillList.Msg
    | InsertSkill of string * option<Map<string, WeaponSkillData>> * option<DicePoolCalculationData>
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceededForSkills of ZeroToFive * DicePoolCalculationData

let init () : MundaneVocationSkills = {
    vocationalSkills = []
    weaponSkillList = []
}

let update msg model : MundaneVocationSkills =
    match msg with
    | VocationalSkillListMsg msg -> {
        model with
            vocationalSkills = VocationalSkillList.update msg model.vocationalSkills
      }
    | WeaponSkillListMsg msg -> {
        model with
            weaponSkillList = WeaponSkillList.update msg model.weaponSkillList
      }
    | InsertSkill(skillName, Some weaponSkillDataMap, dicePoolCalculationDataOption) ->

        match weaponSkillDataMap.TryFind skillName with
        | Some weaponSkillData -> {
            model with
                weaponSkillList =
                    WeaponSkillList.update
                        (WeaponSkillList.InsertWeaponSkill(skillName, weaponSkillData, dicePoolCalculationDataOption))
                        model.weaponSkillList
          }
        | None -> {
            model with
                vocationalSkills =
                    VocationalSkillList.update
                        (VocationalSkillList.InsertVocationalSkill(skillName, dicePoolCalculationDataOption))
                        model.vocationalSkills
          }
    | CalculateDicePools dicePoolCalculationData ->
        let temp vocationalSkills =
            VocationalSkillList.commonVocationalSkillUpdate
                (VocationalSkillList.CalculateVocationalSkillDicePoolList dicePoolCalculationData)
                vocationalSkills

        {
            weaponSkillList = temp model.vocationalSkills
            vocationalSkills = temp model.weaponSkillList
        }
    | CheckIfLevelCapExceededForSkills(levelCap, dicePoolCalculationData) ->
        let temp vocationalSkills =
            List.map
                (fun (vocationalSkill: VocationalSkill) ->
                    VocationalSkill.update
                        (VocationalSkill.SkillMsg(
                            Skill.CheckIfLevelCapExceeded(
                                levelCap,
                                vocationalSkill.governingAttributeNames,
                                dicePoolCalculationData
                            )
                        ))
                        vocationalSkill)
                vocationalSkills

        {
            model with
                vocationalSkills = temp model.vocationalSkills
                weaponSkillList = temp model.weaponSkillList
        }
    | _ -> model

let view attributeNameSet (weaponSkillNameSet: string Set) model dispatch =

    [

        (VocationalSkillList.view attributeNameSet model.vocationalSkills (VocationalSkillListMsg >> dispatch))
        (WeaponSkillList.view attributeNameSet model.weaponSkillList (WeaponSkillListMsg >> dispatch))
        [
            ViewUtils.textInputWithDropdownSet
                (fun input -> InsertSkill(input, None, None) |> dispatch)
                weaponSkillNameSet
                "mundaneVocationSkills"
        ]
    ]
    |> List.collect id