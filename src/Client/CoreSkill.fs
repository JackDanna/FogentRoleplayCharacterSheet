module CoreSkill

open FogentRoleplayLib.CoreSkill
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | SkillMsg of Skill.Msg
    | CalculateSkillDicePool of AttributeName * DicePoolCalculationData

let init coreSkillName governingAttribute dicePoolCalculationData = {
    skill = Skill.init coreSkillName (Set.ofSeq [ governingAttribute ]) dicePoolCalculationData
    governingAttributeName = governingAttribute
}

let update msg model =
    match msg with
    | SkillMsg msg -> {
        model with
            skill = Skill.update msg model.skill
      }
    | CalculateSkillDicePool(attributeName, effects) -> {
        model with
            skill = Skill.update (Skill.Msg.CalculateDicePool(Set.ofSeq [ attributeName ], effects)) model.skill
      }

open Feliz
open Feliz.Bulma

let view model dispatch =
    Skill.view model.skill (SkillMsg >> dispatch) false None