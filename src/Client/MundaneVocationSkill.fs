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
    | SkillMsg msg -> (fun skill -> Skill.update msg skill)
    | CalculateDicePool dicePoolCalculationData ->
        (fun skill -> Skill.update (Skill.CalculateDicePool dicePoolCalculationData) skill)
    | CheckIfLevelCapExceeded checkIfLevelCapExceededData ->
        (fun skill -> Skill.update (Skill.CheckIfLevelCapExceeded checkIfLevelCapExceededData) skill)
    |> processMundaneVocationSkill model

open Feliz
open Feliz.Bulma

let view attributeNameSet model dispatch =
    match model with
    | VocationalSkill skill -> Skill.view attributeNameSet skill (SkillMsg >> dispatch) true true
    | WeaponSkill skill -> Skill.view attributeNameSet skill (SkillMsg >> dispatch) false true