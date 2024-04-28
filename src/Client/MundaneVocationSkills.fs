module MundaneVocationSkills

open FogentRoleplayLib.MundaneVocationSkills

open FogentRoleplayLib.ZeroToFive
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | VocationalSkillListMsg of VocationalSkillList.Msg
    | WeaponSkillListMsg of WeaponSkillList.Msg
    | VocCheckIfLevelCapExceeded of int * ZeroToFive
    | WeaCheckIfLevelCapExceeded of int * ZeroToFive
    | CheckIfLevelCapExceededForAll of ZeroToFive
    | InsertSkill of string * string Set
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
    | InsertSkill(newSkillName, weaponSkillNameSet) ->
        if weaponSkillNameSet.Contains newSkillName then
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
                            (VocationalSkillList.InsertVocationalSkill newSkillName)
                            model.vocationalSkills
            }
    | CheckIfLevelCapExceededForAll zeroToFive ->
        let temp =
            VocationalSkillList.updateCommonVocationalSkill (
                VocationalSkillList.CheckIfLevelCapExeededForAll zeroToFive
            )

        {
            weaponSkillList = temp model.weaponSkillList
            vocationalSkills = temp model.vocationalSkills
        }
    | CalculateDicePools dicePoolCalculationData -> {
        weaponSkillList =
            VocationalSkillList.updateCommonVocationalSkill
                (VocationalSkillList.CalculateDicePools dicePoolCalculationData)
                model.weaponSkillList
        vocationalSkills =
            VocationalSkillList.updateCommonVocationalSkill
                (VocationalSkillList.CalculateDicePools dicePoolCalculationData)
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
            Bulma.input.text [
                prop.list "vocationalSkillNameSet"
                prop.onTextChange (fun input -> InsertSkill(input, weaponSkillNameSet) |> dispatch)
            ]
            Html.datalist [
                prop.id "vocationalSkillNameSet"
                prop.children (
                    weaponSkillNameSet
                    |> Seq.map (fun (itemName: string) -> Html.option [ prop.value itemName ])
                )
            ]
        ]
    ]
    |> List.collect id