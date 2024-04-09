module Character

open FogentRoleplayLib.AttributeStat
open FogentRoleplayLib.CoreSkill

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