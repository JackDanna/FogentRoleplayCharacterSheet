module CoreSkills

open FogentRoleplayLib.CoreSkill
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | ModifiedCoreSkillAtPosition of int * CoreSkill.Msg
    | CalculateCoreSkillDicePools of DicePoolCalculationData

let init coreSkillNameSet governingAttribute effects =
    coreSkillNameSet
    |> Set.map (fun coreSkillName -> CoreSkill.init coreSkillName governingAttribute effects)

let update msg model =
    match msg with
    | ModifiedCoreSkillAtPosition(position, msg) ->
        model
        |> Set.toList
        |> List.mapi (fun index coreSkill ->
            if index = position then
                CoreSkill.update msg coreSkill
            else
                coreSkill)
        |> Set.ofList
    | CalculateCoreSkillDicePools dicePoolCalculationData ->
        Set.map
            (fun coreSkill ->
                CoreSkill.update
                    (CoreSkill.CalculateSkillDicePool(coreSkill.governingAttributeName, dicePoolCalculationData))
                    coreSkill)
            model

open Feliz
open Feliz.Bulma

let view model (dispatch: Msg -> unit) =
    model
    |> Set.toList
    |> List.mapi (fun index coreSkill ->
        CoreSkill.view coreSkill (fun msg -> ModifiedCoreSkillAtPosition(index, msg) |> dispatch)
        |> Bulma.columns
        |> Bulma.content)
    |> Html.ul