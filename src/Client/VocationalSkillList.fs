module VocationalSkillList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.VocationalSkill

type CommonVocationalSkillMsgs =
    | RemoveAtPostion of int
    | ModifiedVocationalSkillAtPosition of int * VocationalSkill.Msg
    | SetEffectDicePoolModLists of DicePoolCalculationData

let commonVocationalSkillUpdate msg model =
    match msg with
    | RemoveAtPostion position -> List.removeAt position model
    | ModifiedVocationalSkillAtPosition(position, msg) ->
        model
        |> List.mapi (fun index vocationSkill ->
            if index = position then
                VocationalSkill.update msg vocationSkill
            else
                vocationSkill)
    | SetEffectDicePoolModLists dicePoolCalculationData ->
        List.map

            (fun vocationSkill ->
                VocationalSkill.update
                    (VocationalSkill.SetEffectDicePoolModList(dicePoolCalculationData))
                    vocationSkill)
            model

open Feliz
open Feliz.Bulma

let viewCommonVocationalSkill
    attributeNameSet
    disableChangeLevel
    (model: VocationalSkill list)
    (dispatch: CommonVocationalSkillMsgs -> unit)
    =
    model
    |> List.mapi (fun position skillRow ->
        VocationalSkill.view
            attributeNameSet
            skillRow
            (fun (msg: VocationalSkill.Msg) -> ((ModifiedVocationalSkillAtPosition(position, msg)) |> dispatch))
            disableChangeLevel
        @ [
            Bulma.column [
                Html.button [ prop.onClick (fun _ -> dispatch (RemoveAtPostion position)); prop.text "-" ]
            ]
        ]
        |> Bulma.columns
        |> Bulma.content)

type Msg =
    | InsertVocationalSkillFromParent of string * DicePoolCalculationData option
    | CommonVocationalSkillMsgs of CommonVocationalSkillMsgs

let update msg model =
    match msg with
    | InsertVocationalSkillFromParent(skillName, dicePoolCalculationDataOption) ->
        match dicePoolCalculationDataOption with
        | Some dicePoolCalculationData ->
            VocationalSkill.init dicePoolCalculationData skillName
            |> List.singleton
            |> List.append model
        | _ -> model
    | CommonVocationalSkillMsgs msg -> commonVocationalSkillUpdate msg model

let view attributeNameSet vocationalSkillNameSet (model: VocationalSkill list) dispatch =
    viewCommonVocationalSkill attributeNameSet true model (CommonVocationalSkillMsgs >> dispatch)