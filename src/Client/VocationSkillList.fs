module VocationSkillList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.VocationSkill
open FogentRoleplayLib.ZeroToFive

type Msg =
    | InsertVocationalSkill of string
    | InsertWeaponSkill of string
    | InsertMagicSkill
    | Remove
    | ModifiedVocationSkillAtPosition of int * VocationSkill.Msg
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceeded of int * ZeroToFive

let init () = [ VocationSkill.init () ]

let update msg model =
    match msg with
    | InsertVocationalSkill name ->
        let temp = VocationalSkill.init ()

        { temp with skill.name = name }
        |> VocationalSkill
        |> List.singleton
        |> List.append model
    | InsertWeaponSkill name ->
        let temp = VocationalSkill.init ()

        { temp with skill.name = name }
        |> WeaponSkill
        |> List.singleton
        |> List.append model
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
    | CheckIfLevelCapExceeded(position, levelCap) ->
        List.mapi
            (fun index vocationSkill ->
                if index = position then
                    VocationSkill.update (VocationSkill.CheckIfLevelCapExceeded(levelCap)) vocationSkill
                else
                    vocationSkill)

            model

open Feliz
open Feliz.Bulma

let view attributeNameSet weaponSkillNameSet (model: VocationSkill list) dispatch =
    List.append
        (List.mapi
            (fun position skillRow ->
                VocationSkill.view attributeNameSet skillRow (fun (msg: VocationSkill.Msg) ->
                    ((ModifiedVocationSkillAtPosition(position, msg)) |> dispatch)))
            model)
        [
            //Html.input [ prop.onClick (fun _ -> dispatch InsertVocationalSkill); prop.text "+" ]
            Bulma.input.text [
                prop.list "weaponSkillNameSet"
                prop.onTextChange (fun input ->
                    if Seq.contains input weaponSkillNameSet then
                        dispatch (InsertWeaponSkill input)
                    else
                        dispatch (InsertVocationalSkill input)

                )
            ]
            Html.datalist [
                prop.id "weaponSkillNameSet"
                prop.children (
                    Seq.map (fun (itemName: string) -> Html.option [ prop.value itemName ]) weaponSkillNameSet
                )
            ]
        // Html.button [ prop.onClick (fun _ -> dispatch Remove); prop.text "-" ]
        ]
    |> Html.ul