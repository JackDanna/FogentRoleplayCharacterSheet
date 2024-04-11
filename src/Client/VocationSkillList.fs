module VocationSkillList

open FogentRoleplayLib.Skill
open FogentRoleplayLib.VocationSkill

type Msg =
    | ModifiedVocationSkillAtPosition of int * VocationSkill.Msg
    | CalculateDicePools of DicePoolCalculationData

// let init () =

let update msg model =
    match msg with
    | ModifiedVocationSkillAtPosition(position, msg) ->
        model
        |> List.mapi (fun index vocationSkill ->
            if index = position then
                VocationSkill.update msg vocationSkill
            else
                vocationSkill)
    | CalculateDicePools dicePoolCalculationData ->
        List.map
            (fun vocationSkill ->
                VocationSkill.update (VocationSkill.CalculateDicePools(dicePoolCalculationData)) vocationSkill)
            model

open Feliz

let view (model: VocationSkill list) dispatch =
    List.append
        (List.mapi
            (fun position skillRow ->
                VocationSkill.view skillRow (fun (msg: VocationSkill.Msg) ->
                    ((ModifiedVocationSkillAtPosition(position, msg)) |> dispatch)))
            model)
        [
        // Html.button [ prop.onClick (fun _ -> dispatch Insert); prop.text "+" ]
        // Html.button [ prop.onClick (fun _ -> dispatch Remove); prop.text "-" ]
        ]
    |> Html.ul