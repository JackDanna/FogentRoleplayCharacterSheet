module Skills

open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | ModifySkillAtPosition of int * Skill.Msg
    | CalculateSkillDicePools of DicePoolCalculationData

let initCoreSkills = FogentRoleplayLib.Skill.initCoreSkills

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

let coreSkillsView
    model
    (dispatch: Msg -> unit)
    (isThisAttributeNameContiainedOnSkill: FogentRoleplayLib.Attribute.Attribute)
    =
    model
    |> Seq.mapi (fun index (coreSkill: FogentRoleplayLib.Skill.Skill) ->
        if coreSkill.governingAttributeNames.Contains isThisAttributeNameContiainedOnSkill.attributeName then
            Skill.coreSkillView coreSkill (fun msg -> ModifySkillAtPosition(index, msg) |> dispatch)
        else
            Html.none)
    |> Html.tbody