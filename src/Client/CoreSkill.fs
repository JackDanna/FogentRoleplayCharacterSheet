module CoreSkill

open FogentRoleplayLib.CoreSkill
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | SkillMsg of Skill.Msg
    | SetEffectDicePoolModList of DicePoolCalculationData

let init dicePoolCalculationData governingAttributeName coreSkillName = {
    skill =
        Skill.init
            coreSkillName
            dicePoolCalculationData.baseDiceEffectList
            (determineCoreSkillEffectDicePoolModList dicePoolCalculationData governingAttributeName)
    governingAttributeName = governingAttributeName
}

let update msg model =
    match msg with
    | SkillMsg msg -> {
        model with
            skill = Skill.update msg model.skill
      }
    | SetEffectDicePoolModList dicePoolCalculationData -> {
        model with
            skill =
                Skill.update
                    (Skill.Msg.SetEffectDicePoolModList(
                        determineCoreSkillEffectDicePoolModList dicePoolCalculationData model.governingAttributeName
                    ))
                    model.skill
      }

open Feliz
open Feliz.Bulma

let view model dispatch =
    Skill.view model.skill (SkillMsg >> dispatch) false None