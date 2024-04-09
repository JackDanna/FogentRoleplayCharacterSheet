module AttributeAndCoreSkills

open FogentRoleplayLib.AttributeAndCoreSkills

type Msg =
    | AttributeMsg of AttributeStat.Msg
    | CoreSkillListMsg of CoreSkillList.Msg

// let init () =

let update msg model =
    match msg with
    | AttributeMsg msg -> {
        model with
            attribute = AttributeStat.update msg model.attribute
      }
    | CoreSkillListMsg msg -> {
        model with
            coreSkills = CoreSkillList.update msg model.coreSkills
      }

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.box [
        AttributeStat.view model.attribute (AttributeMsg >> dispatch)
        CoreSkillList.view model.coreSkills (CoreSkillListMsg >> dispatch)
    ]