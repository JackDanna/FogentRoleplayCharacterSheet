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
let view (coreSkillDicePoolList:DicePool list) (model:CoreSkill list) (dispatch: Msg->unit) (governingAttribute:Attribute)=
    
    Html.ul (
        List.mapi2 
            ( fun index coreSkill coreSkillDicePool->
                if coreSkill.governingAttribute = governingAttribute then
                    CoreSkill.view 
                        coreSkillDicePool 
                        coreSkill
                        (fun msg -> ModifiedCoreSkillAtPosition(index, msg) |> dispatch)
                else
                    Html.none
            )
            model
            coreSkillDicePoolList
    )