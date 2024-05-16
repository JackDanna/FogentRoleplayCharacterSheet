module CoreSkills

open FogentRoleplayLib.CoreSkill

type Msg = ModifiedCoreSkillAtPosition of int * CoreSkill.Msg

let init coreSkillNameSet governingAttribute effects =
    coreSkillNameSet
    |> Set.map (fun coreSkillName -> CoreSkill.init coreSkillName governingAttribute effects)

let update msg model =
    match msg with
    | ModifiedCoreSkillAtPosition(position, msg) ->
        model
        |> Set.toList
        |> List.mapi (fun index coreSkill ->
            if index = position then
                CoreSkill.update msg coreSkill
            else
                coreSkill)
        |> Set.ofList

open Feliz
open Feliz.Bulma

let view (model: CoreSkill list) (dispatch: Msg -> unit) =

    List.mapi
        (fun index coreSkill ->
            CoreSkill.view coreSkill (fun msg -> ModifiedCoreSkillAtPosition(index, msg) |> dispatch)
            |> Bulma.columns
            |> Bulma.content)
        model
    |> Html.ul