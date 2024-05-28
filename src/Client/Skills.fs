module Skills

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.CoreSkillData
open FogentRoleplayLib.AttributeName

type Msg =
    | ModifySkillAtPosition of int * Skill.Msg
    | CalculateCoreSkillDicePools of DicePoolCalculationData

let init skillNameSet governingAttributesNames dicePoolCalculations =
    skillNameSet
    |> Set.map (fun coreSkillName -> Skill.init coreSkillName governingAttributesNames dicePoolCalculations)

let initCoreSkills coreSkillDataSet dicePoolCalculationData =
    Set.map
        (fun (coreSkillData: CoreSkillData) -> Skill.initCoreSkill coreSkillData dicePoolCalculationData)
        coreSkillDataSet

let update msg model =
    match msg with
    | ModifySkillAtPosition(position, msg) ->
        model
        |> Set.toList
        |> List.mapi (fun index coreSkill ->
            if index = position then
                Skill.update msg coreSkill
            else
                coreSkill)
        |> Set.ofList
    | CalculateCoreSkillDicePools dicePoolCalculationData ->
        Set.map (fun coreSkill -> Skill.update (Skill.CalculateDicePool(dicePoolCalculationData)) coreSkill) model

open Feliz
open Feliz.Bulma

// let view attributeNameSet model (dispatch: Msg -> unit) =
//     model
//     |> Set.toList
//     |> List.mapi (fun index coreSkill ->
//         Skill.view attributeNameSet coreSkill (fun msg -> ModifySkillAtPosition(index, msg) |> dispatch) false true
//         |> Bulma.columns
//         |> Bulma.content)
//     |> Html.ul

let coreSkillsView model (dispatch: Msg -> unit) isThisAttributeNameContiainedOnSkill =
    model
    |> Set.toList
    |> List.mapi (fun index (coreSkill: FogentRoleplayLib.Skill.Skill) ->
        if coreSkill.governingAttributeNames.Contains isThisAttributeNameContiainedOnSkill then
            Skill.view Set.empty coreSkill (fun msg -> ModifySkillAtPosition(index, msg) |> dispatch) false true
            |> Bulma.columns
            |> Bulma.content
        else
            Html.none)
    |> Html.ul