module MundaneVocationSkill

open FogentRoleplayLib.MundaneVocationSkill
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.WeaponSkillData

type Msg =
    | SkillMsg of Skill.Msg
    | CalculateDicePool of DicePoolCalculationData
    | CheckIfLevelCapExceeded of Skill.CheckIfLevelCapExceeded

let init (weaponSkillDataMap: Map<string, WeaponSkillData>) dicePoolCalculationData skillName =
    match weaponSkillDataMap.TryFind skillName with
    | Some weaponSkillData ->
        Skill.init weaponSkillData.name weaponSkillData.governingAttributes dicePoolCalculationData
        |> VocationalSkill
    | None -> Skill.init skillName Set.empty dicePoolCalculationData |> WeaponSkill

let processMundaneVocationSkill model operation =
    match model with
    | WeaponSkill skill -> skill |> operation |> WeaponSkill
    | VocationalSkill skill -> skill |> operation |> VocationalSkill


let update msg model =

    match msg with
    | SkillMsg msg -> msg
    | CalculateDicePool dicePoolCalculationData -> (Skill.CalculateDicePool dicePoolCalculationData)
    | CheckIfLevelCapExceeded checkIfLevelCapExceededData -> (Skill.CheckIfLevelCapExceeded checkIfLevelCapExceededData)
    |> (fun msg skill -> Skill.update msg skill)
    |> processMundaneVocationSkill model

open Feliz
open Feliz.Bulma

let vocationalSkillView attributeNameSet model dispatch =
    Skill.viewAsList attributeNameSet model dispatch false true

let weaponSkillView attributeNameSet model dispatch =
    Skill.viewAsList attributeNameSet model dispatch true true

let view attributeNameSet model dispatch =
    match model with
    | VocationalSkill skill -> vocationalSkillView attributeNameSet skill (SkillMsg >> dispatch)
    | WeaponSkill skill -> weaponSkillView attributeNameSet skill (SkillMsg >> dispatch)