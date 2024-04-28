module MagicSkillList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.MagicSkill
open FogentRoleplayLib.ZeroToFive

open FogentRoleplayLib.VocationalSkill

type Msg =
    | InsertMagicSkill of MagicSkillData
    | RemoveAtPostion of int
    | ModifiedVocationSkillAtPosition of int * VocationalSkill.Msg
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceeded of int * ZeroToFive
    | CheckIfLevelCapExeededForAll of ZeroToFive

let init () = []

let update msg model =
    match msg with
    | InsertMagicSkill magicSkillData ->

        let initVocationalSkill = VocationalSkill.init ()

        {
            vocationalSkill = {
                initVocationalSkill with
                    skill.name = magicSkillData.name
            }
            magicSkillData = magicSkillData
        }
        |> List.singleton
        |> List.append model

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
    | CalculateDicePools dicePoolCalculationData ->
        List.map
            (fun magicSkill -> {
                magicSkill with
                    vocationalSkill =
                        VocationalSkill.update
                            (VocationalSkill.CalculateDicePool(dicePoolCalculationData))
                            magicSkill.vocationalSkill
            })
            model
    | CheckIfLevelCapExceeded(position, levelCap) ->
        List.mapi
            (fun index magicSkill ->
                if index = position then
                    {
                        magicSkill with
                            vocationalSkill =
                                VocationalSkill.update
                                    (VocationalSkill.CheckIfLevelCapExceeded(levelCap))
                                    magicSkill.vocationalSkill
                    }
                else
                    magicSkill)

            model
    | CheckIfLevelCapExeededForAll levelCap ->
        model
        |> List.map (fun magicSkill -> {
            magicSkill with
                vocationalSkill =
                    VocationalSkill.update
                        (VocationalSkill.CheckIfLevelCapExceeded levelCap)
                        magicSkill.vocationalSkill
        })

open Feliz
open Feliz.Bulma

let view attributeNameSet (magicSkillDataMap: Map<string, MagicSkillData>) (model: MagicSkill list) dispatch =
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
            Bulma.input.text [
                prop.list "magicSkillNameSet"
                prop.onTextChange (fun input -> magicSkillDataMap.Item input |> InsertMagicSkill |> dispatch)
            ]
            Html.datalist [
                prop.id "magicSkillNameSet"
                prop.children (
                    magicSkillDataMap.Keys
                    |> Seq.map (fun (itemName: string) -> Html.option [ prop.value itemName ])
                )
            ]
        ]