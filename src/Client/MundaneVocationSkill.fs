module MundaneVocationSkill

open FogentRoleplayLib.MundaneVocationSkill
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.WeaponSkillData

type Msg =
    | SkillMsg of Skill.Msg
    | CalculateDicePool of DicePoolCalculationData
    | CheckIfLevelCapExceeded of Skill.ZeroToFiveAndDicePoolCalculationData
    | SetLevelForVocationalSkill of Skill.ZeroToFiveAndDicePoolCalculationData
    | NoOp

let init (weaponSkillDataMap: Map<string, WeaponSkillData>) dicePoolCalculationData skillName =
    match weaponSkillDataMap.TryFind skillName with
    | Some weaponSkillData ->
        Skill.init weaponSkillData.name weaponSkillData.governingAttributes dicePoolCalculationData
        |> WeaponSkill
    | None -> Skill.init skillName Set.empty dicePoolCalculationData |> VocationalSkill

let update msg model =
    match model with
    | WeaponSkill skill ->
        match msg with
        | SkillMsg msg -> msg
        | CalculateDicePool dicePoolCalculationData -> (Skill.CalculateDicePool dicePoolCalculationData)
        | CheckIfLevelCapExceeded checkIfLevelCapExceededData ->
            (Skill.CheckIfLevelCapExceeded checkIfLevelCapExceededData)
        | SetLevelForVocationalSkill _
        | NoOp -> (Skill.NoOp)
        |> (fun msg -> Skill.update msg skill)
        |> WeaponSkill
    | VocationalSkill skill ->
        match msg with
        | SkillMsg msg -> msg
        | CalculateDicePool dicePoolCalculationData -> (Skill.CalculateDicePool dicePoolCalculationData)
        | CheckIfLevelCapExceeded checkIfLevelCapExceededData ->
            (Skill.CheckIfLevelCapExceeded checkIfLevelCapExceededData)
        | SetLevelForVocationalSkill vocaitonStatLevelAndDicePoolCalculationData ->
            (Skill.Msg.ModifySkillLevelWithVocationStatLevel vocaitonStatLevelAndDicePoolCalculationData)
        | NoOp -> (Skill.NoOp)
        |> (fun msg -> Skill.update msg skill)
        |> VocationalSkill

open Feliz
open Feliz.Bulma

let vocationalSkillView attributeNameSet model dispatch =
    Skill.viewAsList attributeNameSet model dispatch true true

let weaponSkillView attributeNameSet model dispatch =
    Skill.viewAsList attributeNameSet model dispatch false true

let view attributeNameSet model dispatch =
    match model with
    | VocationalSkill skill -> vocationalSkillView attributeNameSet skill (SkillMsg >> dispatch)
    | WeaponSkill skill -> weaponSkillView attributeNameSet skill (SkillMsg >> dispatch)