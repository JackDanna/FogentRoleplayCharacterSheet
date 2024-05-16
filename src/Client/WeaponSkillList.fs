module WeaponSkillList

open FogentRoleplayLib.VocationalSkill
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.WeaponSkillData
open VocationalSkillList

type Msg =
    | InsertWeaponSkill of string * WeaponSkillData * option<DicePoolCalculationData>
    | CommonVocationalSkillMsgs of CommonVocationalSkillMsgs

let init () = []

let update msg model =
    match msg with
    | InsertWeaponSkill(skillName, weaponSkillData, Some dicePoolCalculationData) ->
        VocationalSkill.init weaponSkillData.governingAttributes dicePoolCalculationData skillName
        |> List.singleton
        |> List.append model

    | CommonVocationalSkillMsgs msg -> commonVocationalSkillUpdate msg model
    | _ -> model

let view attributeNameSet (model: VocationalSkill list) dispatch =
    viewCommonVocationalSkill attributeNameSet false model (CommonVocationalSkillMsgs >> dispatch)