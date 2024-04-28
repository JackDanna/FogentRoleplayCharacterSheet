module MundaneVocation

open FogentRoleplayLib.MundaneVocation
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | VocationStatMsg of VocationStat.Msg
    | MundaneVocationSkillsMsg of MundaneVocationSkills.Msg
    | CalculateDicePools of DicePoolCalculationData

let init () : MundaneVocation = {
    vocationStat = VocationStat.init ()
    mundaneVocationSkills = MundaneVocationSkills.init ()
}

let update msg (model: MundaneVocation) =
    match msg with
    | VocationStatMsg msg ->
        let newVocationStat = VocationStat.update msg model.vocationStat

        match msg with
        | VocationStat.ZeroToFiveMsg _ -> {
            model with
                vocationStat = newVocationStat
          // mundaneVocationSkills =
          //     MundaneVocationSkills.update
          //         (MundaneVocationSkills.CheckIfLevelCapExeededForAll newVocationStat.level)
          //         model.mundaneVocationSkillList
          }
        | _ -> {
            model with
                vocationStat = newVocationStat
          }

    | MundaneVocationSkillsMsg msg ->

        {
            model with
                mundaneVocationSkills = MundaneVocationSkills.update msg model.mundaneVocationSkills
        }
    | CalculateDicePools msg -> {
        model with
            vocationStat = VocationStat.update (VocationStat.CalculateDicePool msg) model.vocationStat
      // mundaneVocationSkillList =
      //     MundaneVocationSkillList.update
      //         (MundaneVocationSkillList.CalculateDicePools msg)
      //         model.mundaneVocationSkillList
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