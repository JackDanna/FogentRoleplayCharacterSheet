module CoreSkill

open FogentRoleplayLib.DicePool
open FogentRoleplayLib.CoreSkill
open FogentRoleplayLib.Skill

type Msg =
    | Neg1To5Msg of Neg1To5.Msg
    | CalculateDicePool of DicePoolCalculationData

let init () = {
    skill = {
        name = ""
        level = Neg1To5.init ()
        dicePool = baseDicePool
    }
    governingAttributeName = ""
}

let update msg model =
    match msg with
    | Neg1To5Msg msg -> {
        model with
            skill = {
                model.skill with
                    level = Neg1To5.update msg model.skill.level
            }
      }
    | CalculateDicePool msg -> {
        model with
            skill.dicePool = calculateCoreSkillDicePool msg model.skill.level model.governingAttributeName
      }

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.columns [
        Bulma.column [ prop.text model.skill.name ]
        Bulma.column [ Neg1To5.view model.skill.level (Neg1To5Msg >> dispatch) ]
        Bulma.column [ model.skill.dicePool |> dicePoolToString |> prop.text ]
    ]
    |> Bulma.content