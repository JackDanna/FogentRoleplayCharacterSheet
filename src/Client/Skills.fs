module Skills

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.CoreSkillData

type Msg =
    | ModifySkillAtPosition of int * Skill.Msg
    | CalculateSkillDicePools of DicePoolCalculationData

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
    | CalculateSkillDicePools dicePoolCalculationData ->
        Set.map (fun coreSkill -> Skill.update (Skill.CalculateDicePool(dicePoolCalculationData)) coreSkill) model

open Feliz
open Feliz.Bulma

let view attributeNameSet model (dispatch: Msg -> unit) =
    model
    |> Set.toList
    |> List.mapi (fun index coreSkill ->
        Skill.view attributeNameSet coreSkill (fun msg -> ModifySkillAtPosition(index, msg) |> dispatch) false true
        |> Bulma.columns
        |> Bulma.content)
    |> Html.ul

let coreSkillsView model (dispatch: Msg -> unit) isThisAttributeNameContiainedOnSkill =
    model
    |> Set.toList
    |> List.mapi (fun index (coreSkill: FogentRoleplayLib.Skill.Skill) ->
        if coreSkill.governingAttributeNames.Contains isThisAttributeNameContiainedOnSkill then
            Skill.coreSkillView coreSkill (fun msg -> ModifySkillAtPosition(index, msg) |> dispatch)
        else
            Html.none)