module CombatSpeed

open FogentRoleplayLib.CombatSpeed
open FogentRoleplayLib.CombatSpeedCalculation
open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.Skill
open FogentRoleplayLib.Attribute
open FogentRoleplayLib.Neg2To5
open FogentRoleplayLib.SpeedCalculation

type Msg = RecalculateCombatSpeed of Skill Set * Attribute Set

let init skills attributes combatSpeedCalculation : CombatSpeed =
    let numDiceFromGoverningSkill =
        skills
        |> Set.toList
        |> List.find (fun (skill: Skill) -> skill.name = combatSpeedCalculation.governingSkillName)
        |> (fun (skill: Skill) -> dicePoolModListToNumDice skill.dicePoolModList)

    let lvlOfAttributeAsInt =
        findAttributeWithAttributeName attributes combatSpeedCalculation.reactionSpeedAttributeName
        |> (fun attribute -> attribute.level)
        |> neg2To5ToInt

    {

        calculatedSpeed = calculateSpeed numDiceFromGoverningSkill lvlOfAttributeAsInt combatSpeedCalculation.speed
        combatSpeedCalculation = combatSpeedCalculation
        description = combatSpeedCalculationToDescription combatSpeedCalculation

    }

let update msg (model: CombatSpeed) : CombatSpeed =
    match msg with
    | RecalculateCombatSpeed(skills, attributes) -> init skills attributes model.combatSpeedCalculation

open Feliz
open Feliz.Bulma

let view (model: CombatSpeed) = [
    Html.td [ prop.text model.combatSpeedCalculation.name ]
    Html.td [ prop.text (float model.calculatedSpeed) ]
    Html.td [ prop.text model.description ]
]