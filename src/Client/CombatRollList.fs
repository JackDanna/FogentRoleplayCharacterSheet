module CombatRollList

open FogentRoleplayLib.CombatRoll

open FogentRoleplayLib.DicePool
open FogentRoleplayLib.Range
open FogentRoleplayLib.SetAreaOfEffect
open FogentRoleplayLib.ItemElement
open FogentRoleplayLib.Vocation

open FogentRoleplayLib.StringUtils
open FogentRoleplayLib.WeaponSkillData
open FogentRoleplayLib.DicePoolCalculation



type Msg =
    | RecalculateCombatRollList of
        ItemElement List *
        Vocation List *
        Map<string, WeaponSkillData> *
        DicePoolCalculationData

let init () : CombatRoll list = []

let update msg : CombatRoll list =
    match msg with
    | RecalculateCombatRollList(equipmentList, weaponSkillList, weaponSkillDataMap, dicePoolCalculationData) ->
        createCombatRolls equipmentList weaponSkillList weaponSkillDataMap dicePoolCalculationData

open Feliz
open Feliz.Bulma

let combatRollRow (model: CombatRoll) =
    Html.tr [
        Html.td model.itemName
        Html.td model.weaponTypeName
        Html.td model.handedVariation
        Html.td (sprintf "%s (%s)" (model.dicePool |> dicePoolToString) model.weaponAndResourceDicePoolModString)
        Html.td (int model.penetration)
        Html.td (sprintf "%s (%s)" (calculatedRangeToString model.calculatedRange) model.calculatedRange.name)
        Html.td (stringSetToStringSeperatedByCommas model.damageTypeSet)
        Html.td (
            match model.eoName with
            | Some eoName -> eoName
            | None -> ""
            |> sprintf "%d (%s)" model.calculatedEngageableOpponents
        )
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
                        "Item Name"
                        "Weapon Type"
                        "Handed Variation"
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