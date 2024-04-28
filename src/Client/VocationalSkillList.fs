module VocationalSkillList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.VocationalSkill
open FogentRoleplayLib.ZeroToFive

type CommonVocationalSkillMsgs =
    | RemoveAtPostion of int
    | ModifiedVocationalSkillAtPosition of int * VocationalSkill.Msg
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceeded of int * ZeroToFive
    | CheckIfLevelCapExeededForAll of ZeroToFive

let updateCommonVocationSkill msg model =
    match msg with
    | RemoveAtPostion position -> List.removeAt position model
    | ModifiedVocationalSkillAtPosition(position, msg) ->
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

let viewCommonVocationalSkill
    attributeNameSet
    disableChangeLevel
    (model: VocationalSkill list)
    (dispatch: CommonVocationalSkillMsgs -> unit)
    =

    List.mapi
        (fun position skillRow ->
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
        model

type Msg =
    | InsertVocationalSkill of string
    | CommonVocationalSkillMsgs of CommonVocationalSkillMsgs

let init () = []

let update msg model =
    match msg with
    | InsertVocationalSkill name ->
        let initVocationalSkill = VocationalSkill.init ()

        {
            initVocationalSkill with
                skill.name = name
        }
        |> List.singleton
        |> List.append model
    | CommonVocationalSkillMsgs msg -> updateCommonVocationSkill msg model


let view attributeNameSet vocationalSkillNameSet (model: VocationalSkill list) dispatch =
    viewCommonVocationalSkill attributeNameSet true model (CommonVocationalSkillMsgs >> dispatch)