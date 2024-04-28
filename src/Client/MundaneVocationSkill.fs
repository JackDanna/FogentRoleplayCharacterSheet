module MundaneVocationSkills

open FogentRoleplayLib.MundaneVocationSkills

type Msg =
    | VocationalSkillListMsg of VocationalSkillList.Msg
    | WeaponSkillListMsg of WeaponSkillList.Msg

let init () = {
    vocationalSkills = VocationalSkillList.init ()
    weaponSkillList = WeaponSkillList.init ()
}

let update msg model =
    match msg with
    | VocationalSkillListMsg msg -> VocationalSkillList.update msg model.vocationalSkills
    | WeaponSkillListMsg msg -> WeaponSkillList.update msg model.weaponSkillList

let view attributeNameSet vocationalSkillNameSet model dispatch =
    List.append
        (VocationalSkillList.view attributeNameSet Set.empty model.vocationalSkills (VocationalSkillListMsg >> dispatch))
        (WeaponSkillList.view
            attributeNameSet
            vocationalSkillNameSet
            model.weaponSkillList
            (WeaponSkillListMsg >> dispatch))