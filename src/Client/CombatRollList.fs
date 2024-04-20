module CombatRollTable

open FogentRoleplayLib.CombatRoll

open FogentRoleplayLib.Attribute
open FogentRoleplayLib.Vocation

open FogentRoleplayLib.DicePool
open FogentRoleplayLib.Range
open FogentRoleplayLib.SetAreaOfEffect
open FogentRoleplayLib.ItemStack

open FogentRoleplayLib.StringUtils

type CombatRollData = {
    attributeList: Attribute List
    vocationList: Vocation List
    equipmentList: ItemStack List
}

// type Msg =
//     | RecalculateCombatRollList of

let init () : CombatRoll list = []

// let update: CombatRoll list =

//     createCombatRolls
//         magicSkillMap
//         magicCombatMap
//         rangeMap
//         combatRollGoverningAttributeList
//         attributeDeterminedDiceModList
//         equipmentList
//         attributeStatList
//         vocationGroupList

open Feliz
open Feliz.Bulma

let combatRollRow (model: CombatRoll) =
    Html.tr [
        Html.td model.name
        Html.td (dicePoolToString model.dicePool)
        Html.td (int model.penetration)
        Html.td (calculatedRangeToString model.calculatedRange)
        Html.td (stringSetToStringSeperatedByCommas model.damageTypeSet)
        Html.td (int model.calculatedEngageableOpponents)
        Html.td (setAreaOfEffectOptionToString model.setAreaOfEffectOption)
    ]

let view (model: CombatRoll list) =
    Bulma.container [
        Bulma.label "Combat Rolls:"
        Bulma.table [
            table.isBordered
            prop.children [
                Html.thead [
                    [
                        "Name"
                        "Dice Poll"
                        "Penetration"
                        "Effective/MaxRange"
                        "Damage Type"
                        "EO"
                        "AOE"
                    ]
                    |> List.map (fun (thString: string) -> Html.th thString)
                    |> Html.tr
                ]
                Html.tableBody (List.map combatRollRow model)
            ]
        ]
    ]