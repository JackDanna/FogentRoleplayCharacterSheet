module WeaponSkillList

open FogentRoleplayLib.VocationalSkill
open FogentRoleplayLib.DicePoolCalculation
open VocationalSkillList

type Msg =
    | AskParentToInsertWeaponSkill of string
    | InsertWeaponSkillFromParent of string * DicePoolCalculationData
    | CommonVocationalSkillMsgs of CommonVocationalSkillMsgs

let init () = []

let update msg model =
    match msg with
    | AskParentToInsertWeaponSkill _ -> model
    | InsertWeaponSkillFromParent(skillName, dicePoolCalculationData) ->
        VocationalSkill.init dicePoolCalculationData skillName
        |> List.singleton
        |> List.append model
    | CommonVocationalSkillMsgs msg -> commonVocationalSkillUpdate msg model

let view attributeNameSet vocationalSkillNameSet (model: VocationalSkill list) dispatch =
    viewCommonVocationalSkill attributeNameSet false model (CommonVocationalSkillMsgs >> dispatch)