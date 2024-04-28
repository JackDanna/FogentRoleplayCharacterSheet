module WeaponSkillList

open FogentRoleplayLib.VocationalSkill
open VocationalSkillList

type Msg =
    | InsertWeaponSkill of string
    | CommonVocationalSkillMsgs of CommonVocationalSkillMsgs

let init () = []

let update msg model =
    match msg with
    | InsertWeaponSkill name ->
        let initVocationalSkill = VocationalSkill.init ()

        {
            initVocationalSkill with
                skill.name = name
        }
        |> List.singleton
        |> List.append model
    | CommonVocationalSkillMsgs msg -> updateCommonVocationSkill msg model


let view attributeNameSet vocationalSkillNameSet (model: VocationalSkill list) dispatch =
    viewCommonVocationalSkill attributeNameSet false model (CommonVocationalSkillMsgs >> dispatch)