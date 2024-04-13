module CoreSkill

open FogentRoleplayLib.CoreSkill
open FogentRoleplayLib.Skill
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | SkillMsg of Skill.Msg
    | CalculateDicePool of DicePoolCalculationData

let init () = {
    skill = Skill.init ()
    governingAttributeName = ""
}

let update msg model =
    match msg with
    | SkillMsg msg -> {
        model with
            skill = Skill.update msg model.skill
      }
    | CalculateDicePool msg -> {
        model with
            skill =
                Skill.update
                    (Skill.Msg.SetDicePool(
                        calculateCoreSkillDicePool msg model.skill.level model.governingAttributeName
                    ))
                    model.skill
      }

open Feliz
open Feliz.Bulma

let view model dispatch =
    Skill.view model.skill (SkillMsg >> dispatch) false false None