module MagicVocationSkill

open FogentRoleplayLib.MagicVocationSkill
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.Neg1To5

type Msg =
    | MagicSkillMsg of Skill.Msg
    | MundaneVocationSkillMsg of MundaneVocationSkill.Msg
    | CalculateDicePool of DicePoolCalculationData
    | CheckIfLevelCapExceeded of Skill.ZeroToFiveAndDicePoolCalculationData

let initMagicSkill name governingAttributes dicePoolCalculationData =
    Skill.init name Neg1To5.Zero governingAttributes dicePoolCalculationData
    |> MagicSkill

let update msg model =

    match model with
    | MagicSkill skill ->
        match msg with
        | MagicSkillMsg msg -> msg
        | CalculateDicePool dicePoolCalculationData -> Skill.CalculateDicePool dicePoolCalculationData
        | CheckIfLevelCapExceeded checkIfLevelCapExceededData ->
            Skill.CheckIfLevelCapExceeded checkIfLevelCapExceededData
        | MundaneVocationSkillMsg _ -> Skill.NoOp
        |> (fun msg -> Skill.update msg skill)
        |> MagicSkill

    | MundaneVocationSkill mundaneVocationSkill ->
        match msg with
        | MundaneVocationSkillMsg msg -> msg
        | CalculateDicePool dicePoolCalculationData -> MundaneVocationSkill.CalculateDicePool dicePoolCalculationData
        | CheckIfLevelCapExceeded checkIfLevelCapExceededData ->
            MundaneVocationSkill.CheckIfLevelCapExceeded checkIfLevelCapExceededData
        | MagicSkillMsg _ -> MundaneVocationSkill.NoOp
        |> (fun msg -> MundaneVocationSkill.update msg mundaneVocationSkill)
        |> MundaneVocationSkill


open Feliz

let view attributeNameSet model dispatch =
    match model with
    | MagicSkill skill -> Skill.viewAsList attributeNameSet skill (MagicSkillMsg >> dispatch) false true
    | MundaneVocationSkill skill ->
        MundaneVocationSkill.view attributeNameSet skill (MundaneVocationSkillMsg >> dispatch)