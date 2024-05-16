module MagicSkillList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.MagicSkill
open FogentRoleplayLib.AttributeName

type Msg =
    | InsertMagicSkill of
        string *
        option<AttributeName Set> *
        option<Map<string, MagicSkillData>> *
        option<DicePoolCalculationData>
    | RemoveAtPostion of int
    | ModifiedVocationSkillAtPosition of int * VocationalSkill.Msg
    | CalculateMagicSkillDicePool of DicePoolCalculationData

let init () = []

let update msg (model: MagicSkill Set) =
    match msg with
    | InsertMagicSkill(skillName,
                       Some magicVocationGoverningAttributeNames,
                       Some magicSkillDataMap,
                       Some dicePoolCalculationData) ->

        match magicSkillDataMap.TryFind skillName with
        | None -> model
        | Some magicSkillData ->

            {
                vocationalSkill =
                    VocationalSkill.init magicVocationGoverningAttributeNames dicePoolCalculationData skillName
                magicSkillData = magicSkillData
            }
            |> Set.singleton
            |> Set.union model

    | RemoveAtPostion position -> model |> Set.toSeq |> Seq.removeAt position |> Set.ofSeq
    | ModifiedVocationSkillAtPosition(position, msg) ->
        model
        |> Set.toSeq
        |> Seq.mapi (fun index magicSkill ->
            if index = position then
                {
                    magicSkill with
                        vocationalSkill = VocationalSkill.update msg magicSkill.vocationalSkill
                }
            else
                magicSkill)
        |> Set.ofSeq
    | CalculateMagicSkillDicePool dicePoolCalculationData ->
        Set.map
            (fun magicSkill -> {
                magicSkill with
                    vocationalSkill =
                        VocationalSkill.update
                            (VocationalSkill.CalculateVocationalSkillDicePool(dicePoolCalculationData))
                            magicSkill.vocationalSkill
            })
            model
    | _ -> model

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
                (fun input -> InsertMagicSkill(input, None, None, None) |> dispatch)
                magicSkillNamesSet
                "magiSkillList"
        ]