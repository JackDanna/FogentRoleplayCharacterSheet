module CoreSkillList

open FogentRoleplayLib.CoreSkill
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.Attribute

type Msg = ModifiedCoreSkillAtPosition of int * CoreSkill.Msg

let init () = [CoreSkill.init();CoreSkill.init()]

let update msg (model: CoreSkill list) =
    match msg with
    | ModifiedCoreSkillAtPosition (position, msg) ->
        model
        |> List.mapi ( fun index coreSkill  ->
            if index = position then
                CoreSkill.update msg coreSkill
            else
                coreSkill
        )

open Feliz
open Feliz.Bulma
let view (model:CoreSkill list) (dispatch: Msg->unit) =


    Html.ul (
        List.mapi 
            ( fun index coreSkill ->
                CoreSkill.view 
                        coreSkill
                        (fun msg -> ModifiedCoreSkillAtPosition(index, msg) |> dispatch)
            )
            model
    )

        
