module AttributeAndCoreSkills

open FogentRoleplayLib.AttributeAndCoreSkills
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | AttributeMsg of AttributeStat.Msg
    | CoreSkillListMsg of CoreSkillList.Msg
    | CalculateDicePool of DicePoolCalculationData

// let init () =

let update msg model =
    match msg with
    | AttributeMsg msg -> {
        model with
            attributeStat = AttributeStat.update msg model.attributeStat
      }
    | CoreSkillListMsg msg -> {
        model with
            coreSkills = CoreSkillList.update msg model.coreSkills
      }
    | CalculateDicePool msg -> {
        model with
            coreSkills = CoreSkillList.update (CoreSkillList.Msg.CalculateDicePools(msg)) model.coreSkills
      }

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.box [
        AttributeStat.view model.attributeStat (AttributeMsg >> dispatch)
        CoreSkillList.view model.coreSkills (CoreSkillListMsg >> dispatch)
    ]