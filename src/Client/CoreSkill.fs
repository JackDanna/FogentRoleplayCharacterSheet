module CoreSkill

open FogentRoleplayLib.DicePool
open FogentRoleplayLib.CoreSkill

type Msg = Neg1To5Msg of Neg1To5.Msg

let init () =
    {
        skill = { name = "Lift"; level = Neg1To5.init()}
        governingAttribute = ""
    }

let update msg model =
    match msg with
    | Neg1To5Msg msg ->
        { model with
            skill = { 
                level = Neg1To5.update msg model.skill.level
                name = model.skill.name
            }
        }

open Feliz
open Feliz.Bulma

let view (coreSkillDicePool: DicePool)  model dispatch =
    Bulma.columns [
        Bulma.column [
            prop.text model.skill.name
        ]
        Bulma.column [
            Neg1To5.view model.skill.level (Neg1To5Msg >> dispatch)
        ]
        Bulma.column [
            coreSkillDicePool
            |> dicePoolToString
            |> prop.text
        ]
    ]
    |> Bulma.content