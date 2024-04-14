module VocationSkillList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.VocationSkill
open FogentRoleplayLib.ZeroToFive

type Msg =
    | InsertVocationalSkill
    | InsertWeaponSkill
    | InsertMagicSkill
    | Remove
    | ModifiedVocationSkillAtPosition of int * VocationSkill.Msg
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceeded of ZeroToFive

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
    | CheckIfLevelCapExceeded levelCap ->
        List.map
            (fun vocationalSkill ->
                VocationSkill.update (VocationSkill.CheckIfLevelCapExceeded(levelCap)) vocationalSkill)
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