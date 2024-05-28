module MundaneVocationSkill

open FogentRoleplayLib.MundaneVocationSkill
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | SkillMsg of Skill.Msg
    | CalculateDicePool of DicePoolCalculationData
    | CheckIfLevelCapExceeded of Skill.CheckIfLevelCapExceeded


let initVocationalSkill name governingAttributes dicePoolCalculationData =
    Skill.init name governingAttributes dicePoolCalculationData |> VocationalSkill

let initWeaponSkill name governingAttributes dicePoolCalculationData =
    Skill.init name governingAttributes dicePoolCalculationData |> WeaponSkill

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