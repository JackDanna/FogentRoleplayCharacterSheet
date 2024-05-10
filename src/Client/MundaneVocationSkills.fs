module MundaneVocationSkills

open FogentRoleplayLib.MundaneVocationSkills

open FogentRoleplayLib.ZeroToFive
open FogentRoleplayLib.DicePoolCalculation

type InsetSkillMsgData = {
    skillName: string
    dicePoolCalculationData: DicePoolCalculationData option
    weaponSkillNameSet: string Set option
}

type Msg =
    | VocationalSkillListMsg of VocationalSkillList.Msg
    | WeaponSkillListMsg of WeaponSkillList.Msg
    | VocCheckIfLevelCapExceeded of int * ZeroToFive
    | WeaCheckIfLevelCapExceeded of int * ZeroToFive
    | CheckIfLevelCapExceededForAll of ZeroToFive
    | InsertSkill of InsetSkillMsgData
    | CalculateDicePools of DicePoolCalculationData

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
    | InsertSkill insertSkillMsgData ->
        if insertSkillMsgData.weaponSkillNameSet.Contains newSkillName then
            {
                model with
                    weaponSkillList =
                        WeaponSkillList.update (WeaponSkillList.InsertWeaponSkill newSkillName) model.weaponSkillList
            }
        else
            {
                model with
                    vocationalSkills =
                        VocationalSkillList.update
                            (VocationalSkillList.AskParentToInsertVocationalSkill(
                                newSkillName,
                                dicePoolCalculationDataOption,
                                attributeNameSetOption
                            ))
                            model.vocationalSkills
            }
    | CheckIfLevelCapExceededForAll zeroToFive ->
        let temp =
            VocationalSkillList.commonVocationalSkillUpdate (
                VocationalSkillList.CheckIfLevelCapExeededForAll zeroToFive
            )

        {
            weaponSkillList = temp model.weaponSkillList
            vocationalSkills = temp model.vocationalSkills
        }
    | CalculateDicePools dicePoolCalculationData -> {
        weaponSkillList =
            VocationalSkillList.commonVocationalSkillUpdate
                (VocationalSkillList.SetEffectDicePoolModLists dicePoolCalculationData)
                model.weaponSkillList
        vocationalSkills =
            VocationalSkillList.commonVocationalSkillUpdate
                (VocationalSkillList.SetEffectDicePoolModLists dicePoolCalculationData)
                model.vocationalSkills
      }

//| VocCheckIfLevelCapExceeded (position, vocationLevel) ->
//    VocationalSkillList.update (VocationalSkillList.ModifiedVocationalSkillAtPosition (position,vocationLevel))

open Feliz
open Feliz.Bulma

let view attributeNameSet (weaponSkillNameSet: string Set) model dispatch =

    [

        (VocationalSkillList.view attributeNameSet Seq.empty model.vocationalSkills (VocationalSkillListMsg >> dispatch))
        (WeaponSkillList.view attributeNameSet Seq.empty model.weaponSkillList (WeaponSkillListMsg >> dispatch))
        [
            ViewUtils.textInputWithDropdownSet
                (fun input -> InsertSkill(input, weaponSkillNameSet) |> dispatch)
                weaponSkillNameSet
                "mundaneVocationSkills"
        ]
    ]
    |> List.collect id