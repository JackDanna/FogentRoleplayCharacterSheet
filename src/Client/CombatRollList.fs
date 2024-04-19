module CombatRollTable

open FogentRoleplayLib.CombatRoll

open FogentRoleplayLib.Attribute
open FogentRoleplayLib.Vocation

open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.Range
open FogentRoleplayLib.SetAreaOfEffect
open FogentRoleplayLib.ItemStack

open FogentRoleplayLib.MagicSkill
open FogentRoleplayLib.AttributeDeterminedDiceModEffect

type CombatRollData = {
    attributeList: Attribute List
    vocationList: Vocation List
    equipmentList: ItemStack List
}

// type Msg =
//     | RecalculateCombatRollList of

let init () : CombatRoll list = []

let update: CombatRoll list =

    createCombatRolls
        magicSkillMap
        magicCombatMap
        rangeMap
        combatRollGoverningAttributeList
        attributeDeterminedDiceModList
        equipmentList
        attributeStatList
        vocationGroupList

open Feliz
open Feliz.Bulma

let combatRollRow (combatRoll: CombatRoll) =
    Html.tr [
        Html.td combatRoll.name
        Html.td (dicePoolToString combatRoll.dicePool)
        Html.td (int combatRoll.penetration)
        Html.td (calculatedRangeToString combatRoll.calculatedRange)
        Html.td (damageTypesToString combatRoll.damageTypes)
        Html.td (int combatRoll.engageableOpponents)
        Html.td (calculatedAOEOptionToString combatRoll.areaOfEffectShape)
    ]

let view (model: CombatRoll list) =
    Bulma.container [
        Bulma.label "Combat Rolls:"
        Bulma.table [
            table.isBordered
            prop.children [
                Html.thead [
                    List.map (fun (thString: string) -> Html.th thString) [
                        "Name" "Dice Poll" "Penetration" "Effective/MaxRange" "Damage Type" "EO" "AOE"
                    ]
                    |> Html.tr
                ]
                Html.tableBody (List.map combatRollRow model)
            ]
        ]
    ]