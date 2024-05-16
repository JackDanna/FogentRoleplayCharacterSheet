module VocationSkillList

open FogentRoleplayLib.Effects
open FogentRoleplayLib.VocationalSkill
open FogentRoleplayLib.ZeroToFive

type CommonVocationalSkillMsgs =
    | RemoveAtPostion of int
    | ModifiedVocationSkillAtPosition of int * VocationalSkill.Msg
    | CalculateDicePools of Effects
    | CheckIfLevelCapExceeded of int * ZeroToFive
    | CheckIfLevelCapExeededForAll of ZeroToFive

let init () = []

let commonVocationSkillUpdate msg model =
    match msg with
    | RemoveAtPostion position -> List.removeAt position model
    | ModifiedVocationSkillAtPosition(position, msg) ->
        model
        |> List.mapi (fun index vocationSkill ->
            if index = position then
                VocationalSkill.update msg vocationSkill
            else
                vocationSkill)
    | CalculateDicePools dicePoolCalculationData ->
        List.map
            (fun vocationSkill ->
                VocationalSkill.update (VocationalSkill.CalculateDicePool(dicePoolCalculationData)) vocationSkill)
            model
    | CheckIfLevelCapExceeded(position, levelCap) ->
        List.mapi
            (fun index vocationSkill ->
                if index = position then
                    VocationalSkill.update (VocationalSkill.CheckIfLevelCapExceeded(levelCap)) vocationSkill
                else
                    vocationSkill)

            model
    | CheckIfLevelCapExeededForAll levelCap ->
        model
        |> List.map (fun vocationSkill ->
            VocationalSkill.update (VocationalSkill.CheckIfLevelCapExceeded levelCap) vocationSkill)

open Feliz
open Feliz.Bulma

let view
    attributeNameSet
    vocationSkillNameSet
    onVocationalSkillChange
    model
    (dispatch: CommonVocationalSkillMsgs -> unit)
    =

    List.append
        (List.mapi
            (fun position skillRow ->
                VocationalSkill.view
                    attributeNameSet
                    skillRow
                    (fun (msg: VocationalSkill.Msg) -> ((ModifiedVocationSkillAtPosition(position, msg)) |> dispatch))
                    true
                @ [
                    Bulma.column [
                        Html.button [ prop.onClick (fun _ -> dispatch (RemoveAtPostion position)); prop.text "-" ]
                    ]
                ]
                |> Bulma.columns
                |> Bulma.content)
            model)
        [
            Bulma.input.text [ prop.list "vocationSkillNameSet"; prop.onTextChange onVocationalSkillChange ]
            Html.datalist [
                prop.id "vocationSkillNameSet"
                prop.children (
                    vocationSkillNameSet
                    |> Seq.map (fun (itemName: string) -> Html.option [ prop.value itemName ])
                )
            ]
        ]
    |> Html.ul