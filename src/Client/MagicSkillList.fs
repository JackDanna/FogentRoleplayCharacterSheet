module MagicSkillList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.MagicSkill

type Msg =
    | InsertMagicSkill of string * option<DicePoolCalculationData> * option<Map<string, MagicSkillData>>
    | RemoveAtPostion of int
    | ModifiedVocationSkillAtPosition of int * VocationalSkill.Msg

let init () = []

let update msg model =
    match msg with
    | InsertMagicSkill(skillName, dicePoolCalculationDataOption, magicSkillDataMapOption) ->


        match dicePoolCalculationDataOption, magicSkillDataMapOption with
        | Some dicePoolCalcualtionData, Some magicSkillDataMap ->
            match magicSkillDataMap.TryFind skillName with
            | None -> model
            | Some magicSkillData ->

                {
                    vocationalSkill = VocationalSkill.init dicePoolCalcualtionData skillName
                    magicSkillData = magicSkillData
                }
                |> List.singleton
                |> List.append model
        | _, _ -> model

    | RemoveAtPostion position -> List.removeAt position model
    | ModifiedVocationSkillAtPosition(position, msg) ->
        model
        |> List.mapi (fun index magicSkill ->
            if index = position then
                {
                    magicSkill with
                        vocationalSkill = VocationalSkill.update msg magicSkill.vocationalSkill
                }
            else
                magicSkill)

open Feliz
open Feliz.Bulma

let view attributeNameSet (magicSkillNamesSet: string Set) (model: MagicSkill list) dispatch =
    List.append
        (List.mapi
            (fun position (skillRow: MagicSkill) ->
                VocationalSkill.view
                    attributeNameSet
                    skillRow.vocationalSkill
                    (fun (msg: VocationalSkill.Msg) -> ((ModifiedVocationSkillAtPosition(position, msg)) |> dispatch))
                    false
                @ [
                    Bulma.column [
                        Html.button [ prop.onClick (fun _ -> dispatch (RemoveAtPostion position)); prop.text "-" ]
                    ]
                ]
                |> Bulma.columns
                |> Bulma.content)
            model)
        [
            ViewUtils.textInputWithDropdownSet
                (fun input -> InsertMagicSkill(input, None, None) |> dispatch)
                magicSkillNamesSet
                "magiSkillList"
        ]