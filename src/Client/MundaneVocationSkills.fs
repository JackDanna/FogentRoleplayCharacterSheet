module MundaneVocationSkills

open FogentRoleplayLib.MundaneVocationSkills

open FogentRoleplayLib.ZeroToFive

type Msg =
    | VocationalSkillListMsg of VocationalSkillList.Msg
    | WeaponSkillListMsg of WeaponSkillList.Msg
    | VocCheckIfLevelCapExceeded of int * ZeroToFive
    | WeaCheckIfLevelCapExceeded of int * ZeroToFive
    | CheckIfLevelCapExceededForAll of ZeroToFive

let init () : MundaneVocationSkills = {
    vocationalSkills = []
    weaponSkillList = []
}

let update msg model : MundaneVocationSkills =
    match msg with
    | VocationalSkillListMsg msg -> {
        model with
            vocationalSkills = VocationalSkillList.update msg model.vocationalSkills
      }
    | WeaponSkillListMsg msg -> {
        model with
            weaponSkillList = WeaponSkillList.update msg model.weaponSkillList
      }
//| VocCheckIfLevelCapExceeded (position, vocationLevel) ->
//    VocationalSkillList.update (VocationalSkillList.ModifiedVocationalSkillAtPosition (position,vocationLevel))


let view attributeNameSet weaponSkillNames model dispatch =
    List.append
        (VocationalSkillList.view attributeNameSet Seq.empty model.vocationalSkills (VocationalSkillListMsg >> dispatch))
        (WeaponSkillList.view attributeNameSet Seq.empty model.weaponSkillList (WeaponSkillListMsg >> dispatch))