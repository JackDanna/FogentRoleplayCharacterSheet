module CombatSpeed

open FogentRoleplayLib.CombatSpeed
open FogentRoleplayLib.CombatSpeedCalculation
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.Skill
open FogentRoleplayLib.Attribute
open FogentRoleplayLib.Neg2To5
open FogentRoleplayLib.SpeedCalculation

type Msg = RecalculateCombatSpeed of Skill list * Attribute Set

let init skills attributes combatSpeedCalculation : CombatSpeed =
    let numDiceFromGoverningSkill =
        List.find (fun (skill: Skill) -> skill.name = combatSpeedCalculation.governingSkillName) skills
        |> (fun (skill: Skill) -> dicePoolToNumDice skill.dicePool)

    let lvlOfAttributeAsInt =
        findAttributeWithAttributeName attributes combatSpeedCalculation.reactionSpeedAttributeName
        |> (fun attribute -> attribute.level)
        |> neg2To5ToInt

    {

        calculatedSpeed = calculateSpeed numDiceFromGoverningSkill lvlOfAttributeAsInt combatSpeedCalculation.speed
        combatSpeedCalculation = combatSpeedCalculation
    }

let update msg (model: CombatSpeed) : CombatSpeed =
    match msg with
    | RecalculateCombatSpeed(skills: Skill list, attributes) -> init skills attributes model.combatSpeedCalculation

open Feliz
open Feliz.Bulma

let view (model: CombatSpeed) = [
    Html.td [ prop.text model.combatSpeedCalculation.name ]
    Html.td [ prop.text (float model.calculatedSpeed) ]
]