module VocationSkillList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.VocationSkill
open FogentRoleplayLib.ZeroToFive

open FogentRoleplayLib.Character

open FogentRoleplayLib.MagicSkill

type Msg =
    | InsertVocationalSkill of string
    | InsertWeaponSkill of string
    | InsertMagicSkill of string * MagicSkillData
    | RemoveAtPostion of int
    | ModifiedVocationSkillAtPosition of int * VocationSkill.Msg
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceeded of int * ZeroToFive

let init () = []

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
    //| InsertMagicSkill -> model
    | RemoveAtPostion position -> List.removeAt position model
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

let view attributeNameSet vocationSkillData (model: VocationSkill list) dispatch =
    List.append
        (List.mapi
            (fun position skillRow ->
                VocationSkill.view attributeNameSet skillRow (fun (msg: VocationSkill.Msg) ->
                    ((ModifiedVocationSkillAtPosition(position, msg)) |> dispatch))
                @ [
                    Bulma.column [
                        Html.button [ prop.onClick (fun _ -> dispatch (RemoveAtPostion position)); prop.text "-" ]
                    ]
                ]
                |> Bulma.columns
                |> Bulma.content)
            model)
        [
            //Html.input [ prop.onClick (fun _ -> dispatch InsertVocationalSkill); prop.text "+" ]
            Bulma.input.text [
                prop.list "weaponSkillNameSet"
                prop.onTextChange (fun input ->
                    if Seq.contains input vocationSkillData.magicSkillDataMap.Keys then
                        (input, (vocationSkillData.magicSkillDataMap.Item input)) |> InsertMagicSkill
                    elif Seq.contains input vocationSkillData.weaponGoverningSkillNameSet then
                        InsertWeaponSkill input
                    else
                        InsertVocationalSkill input
                    |> dispatch)
            ]
            Html.datalist [
                prop.id "weaponSkillNameSet"
                prop.children (
                    Seq.map
                        (fun (itemName: string) -> Html.option [ prop.value itemName ])
                        vocationSkillData.magicSkillDataMap.Keys
                )
            ]
        ]
    |> Html.ul