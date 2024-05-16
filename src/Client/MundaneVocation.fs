module MundaneVocation

open FogentRoleplayLib.MundaneVocation
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | VocationStatMsg of VocationStat.Msg
    | MundaneVocationSkillsMsg of MundaneVocationSkills.Msg
    | CalculateDicePools of DicePoolCalculationData

let init name dicePoolCalculationData : MundaneVocation = {
    vocationStat = VocationStat.init name dicePoolCalculationData
    mundaneVocationSkills = MundaneVocationSkills.init ()
}

let update msg (model: MundaneVocation) =
    match msg with
    | VocationStatMsg msg ->
        let newVocationStat = VocationStat.update msg model.vocationStat

        match msg with
        | VocationStat.ZeroToFiveMsg(_, Some dicePoolCalculationData) ->


            {
                model with
                    vocationStat = newVocationStat
                    mundaneVocationSkills =
                        MundaneVocationSkills.update
                            (MundaneVocationSkills.CheckIfLevelCapExceededForSkills(
                                newVocationStat.level,
                                dicePoolCalculationData
                            ))
                            model.mundaneVocationSkills
            }
        | _ -> {
            model with
                vocationStat = newVocationStat
          }

    | MundaneVocationSkillsMsg msg ->

        match msg with
        | MundaneVocationSkills.VocationalSkillListMsg(VocationalSkillList.CommonVocationalSkillMsgs(VocationalSkillList.ModifiedVocationalSkillAtPosition(pos,
                                                                                                                                                           VocationalSkill.SkillMsg(Skill.ModifySkillLevel(msg,
                                                                                                                                                                                                           _,
                                                                                                                                                                                                           attributeOption,
                                                                                                                                                                                                           dicePoolCalculationDataOption))))) ->
            MundaneVocationSkills.VocationalSkillListMsg(
                VocationalSkillList.CommonVocationalSkillMsgs(
                    VocationalSkillList.ModifiedVocationalSkillAtPosition(
                        pos,
                        VocationalSkill.SkillMsg(
                            Skill.ModifySkillLevel(
                                msg,
                                (Some model.vocationStat.level),
                                attributeOption,
                                dicePoolCalculationDataOption
                            )
                        )
                    )
                )
            )
        | MundaneVocationSkills.WeaponSkillListMsg(WeaponSkillList.CommonVocationalSkillMsgs(VocationalSkillList.ModifiedVocationalSkillAtPosition(pos,
                                                                                                                                                   VocationalSkill.SkillMsg(Skill.ModifySkillLevel(msg,
                                                                                                                                                                                                   _,
                                                                                                                                                                                                   attributeOption,
                                                                                                                                                                                                   dicePoolCalculationDataOption))))) ->
            MundaneVocationSkills.WeaponSkillListMsg(
                WeaponSkillList.CommonVocationalSkillMsgs(
                    VocationalSkillList.ModifiedVocationalSkillAtPosition(
                        pos,
                        VocationalSkill.SkillMsg(
                            Skill.ModifySkillLevel(
                                msg,
                                (Some model.vocationStat.level),
                                attributeOption,
                                dicePoolCalculationDataOption
                            )
                        )
                    )
                )
            )
        | _ -> msg
        |> (fun msg -> {
            model with
                mundaneVocationSkills = MundaneVocationSkills.update msg model.mundaneVocationSkills
        })
    | CalculateDicePools msg -> {
        model with
            vocationStat = VocationStat.update (VocationStat.CalculateDicePool msg) model.vocationStat
            mundaneVocationSkills =
                MundaneVocationSkills.update (MundaneVocationSkills.CalculateDicePools msg) model.mundaneVocationSkills
      }

open Feliz
open Feliz.Bulma

let view attributeNameSet (weaponSkillNames) (model: MundaneVocation) dispatch =
    [
        VocationStat.view attributeNameSet model.vocationStat (VocationStatMsg >> dispatch)
    ]
    @ MundaneVocationSkills.view
        attributeNameSet
        weaponSkillNames
        model.mundaneVocationSkills
        (MundaneVocationSkillsMsg >> dispatch)