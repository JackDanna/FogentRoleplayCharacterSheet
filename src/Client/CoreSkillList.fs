module CoreSkillList

open FogentRoleplayLib.CoreSkill
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | ModifiedCoreSkillAtPosition of int * CoreSkill.Msg
    | SetCoreSkillEffectDicePoolMods of DicePoolCalculationData

let init dicePoolCalculationData governingAttributeName coreSkillNameSet =
    coreSkillNameSet
    |> Set.map (fun coreSkillName -> CoreSkill.init dicePoolCalculationData governingAttributeName coreSkillName)

let update msg (model: CoreSkill list) =
    match msg with
    | ModifiedCoreSkillAtPosition(position, msg) ->
        model
        |> List.mapi (fun index coreSkill ->
            if index = position then
                CoreSkill.update msg coreSkill
            else
                coreSkill)
    | SetCoreSkillEffectDicePoolMods dicePoolCalculationData ->
        List.map
            (fun coreSkill ->
                CoreSkill.update (CoreSkill.Msg.SetEffectDicePoolModList dicePoolCalculationData) coreSkill)
            model

open Feliz
open Feliz.Bulma

let view (model: CoreSkill list) (dispatch: Msg -> unit) =

    List.mapi
        (fun index coreSkill ->
            CoreSkill.view coreSkill (fun msg -> ModifiedCoreSkillAtPosition(index, msg) |> dispatch)
            |> Bulma.columns
            |> Bulma.content)
        model
    |> Html.ul