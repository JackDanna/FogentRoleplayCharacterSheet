module MagicVocationSkill

open FogentRoleplayLib.MagicVocationSkill
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | SkillMsg of Skill.Msg
    | MundaneVocationSkillMsg of MundaneVocationSkill.Msg
    | CalculateDicePool of DicePoolCalculationData
    | CheckIfLevelCapExceeded of Skill.CheckIfLevelCapExceeded

let initMagicSkill name governingAttributes dicePoolCalculationData =
    Skill.init name governingAttributes dicePoolCalculationData |> MagicSkill

let processMagicVocationSkill model operation =
    match model with
    | MagicSkill skill -> skill |> operation |> MagicSkill
    | MundaneVocationSkill mundaneVocationSkill ->
        MundaneVocationSkill.processMundaneVocationSkill mundaneVocationSkill operation
        |> MundaneVocationSkill

let update msg model =

    match msg with
    | SkillMsg msg -> processMagicVocationSkill model (fun skill -> Skill.update msg skill)
    | MundaneVocationSkillMsg(MundaneVocationSkill.SkillMsg msg) ->
        processMagicVocationSkill model (fun skill -> Skill.update msg skill)
    | CalculateDicePool dicePoolCalculationData ->
        processMagicVocationSkill model (fun skill ->
            Skill.update (Skill.CalculateDicePool dicePoolCalculationData) skill)
    | CheckIfLevelCapExceeded checkIfLevelCapExceededData ->
        processMagicVocationSkill model (fun skill ->
            Skill.update (Skill.CheckIfLevelCapExceeded checkIfLevelCapExceededData) skill)
    | _ -> model


open Feliz
open Feliz.Bulma

let view attributeNameSet model dispatch =
    match model with
    | MagicSkill skill -> Skill.viewAsList attributeNameSet skill (SkillMsg >> dispatch) false true
    | MundaneVocationSkill skill ->
        MundaneVocationSkill.view attributeNameSet skill (MundaneVocationSkillMsg >> dispatch)