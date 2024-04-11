module VocationalSkill

open FogentRoleplayLib.VocationalSkill
open FogentRoleplayLib.Skill
open FogentRoleplayLib.StringUtils

type Msg =
    | SkillMsg of Skill.Msg
    | CalculateDicePool of DicePoolCalculationData

let init () = {
    skill = Skill.init ()
    governingAttributeNames = []
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
                        calculateVocationalSkillDicePool msg model.skill.level model.governingAttributeNames
                    ))
                    model.skill
      }

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.column [
        model.governingAttributeNames
        |> stringListToStringSeperatedByCommas
        |> prop.text
    ]
    |> Some
    |> Skill.view model.skill (SkillMsg >> dispatch)