module VocationSkillList

open FogentRoleplayLib.Skill
open FogentRoleplayLib.VocationSkill

type Msg =
    | InsertVocationalSkill
    | InsertWeaponSkill
    | InsertMagicSkill
    | Remove
    | ModifiedVocationSkillAtPosition of int * VocationSkill.Msg
    | CalculateDicePools of DicePoolCalculationData

let init () = [ VocationSkill.init () ]

let update msg model =
    match msg with
    | InsertVocationalSkill -> List.append model [ VocationSkill.init () ]
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

let view attributeNameSet (model: VocationSkill list) dispatch =
    List.append
        (List.mapi
            (fun position skillRow ->
                VocationSkill.view attributeNameSet skillRow (fun (msg: VocationSkill.Msg) ->
                    ((ModifiedVocationSkillAtPosition(position, msg)) |> dispatch)))
            model)
        [
            Html.button [ prop.onClick (fun _ -> dispatch InsertVocationalSkill); prop.text "+" ]
        // Html.button [ prop.onClick (fun _ -> dispatch Remove); prop.text "-" ]
        ]
    |> Html.ul