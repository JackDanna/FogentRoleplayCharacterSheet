module CoreSkillList

open FogentRoleplayLib.CoreSkill
open FogentRoleplayLib.Skill

type Msg =
    | ModifiedCoreSkillAtPosition of int * CoreSkill.Msg
    | CalculateCoreSkillDicePools of DicePoolCalculationData

let init () = [ CoreSkill.init (); CoreSkill.init () ]

let update msg (model: CoreSkill list) =
    match msg with
    | ModifiedCoreSkillAtPosition(position, msg) ->
        model
        |> List.mapi (fun index coreSkill ->
            if index = position then
                CoreSkill.update msg coreSkill
            else
                coreSkill)
    | CalculateCoreSkillDicePools dicePoolCalculationData ->
        List.map
            (fun coreSkill -> CoreSkill.update (CoreSkill.Msg.CalculateDicePool dicePoolCalculationData) coreSkill)
            model

open Feliz

let view (model: CoreSkill list) (dispatch: Msg -> unit) =

    List.mapi
        (fun index coreSkill ->
            CoreSkill.view coreSkill (fun msg -> ModifiedCoreSkillAtPosition(index, msg) |> dispatch))
        model
    |> Html.ul