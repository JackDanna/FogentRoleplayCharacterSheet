module AttributeAndCoreSkillsList

open FogentRoleplayLib.Character

type Msg = ModifyAttributeAndCoreSkillsList of int * AttributeAndCoreSkills.Msg

let update msg model =
    match msg with
    | ModifyAttributeAndCoreSkillsList(position, msg) ->
        List.mapi
            (fun index attributeAndCoreSkills ->
                if index = position then
                    AttributeAndCoreSkills.update msg attributeAndCoreSkills
                else
                    attributeAndCoreSkills)
            model

open Feliz
open Feliz.Bulma

let view model dispatch = Html.none