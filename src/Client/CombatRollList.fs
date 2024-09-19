module CombatRollList

open FogentRoleplayLib.CombatRoll

open FogentRoleplayLib.DicePool
open FogentRoleplayLib.CalculatedRange
open FogentRoleplayLib.SetAreaOfEffect
open FogentRoleplayLib.ItemElement
open FogentRoleplayLib.Vocation

open FogentRoleplayLib.StringUtils
open FogentRoleplayLib.WeaponSkillData
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.WeaponSpell



type Msg =
    | RecalculateCombatRollList of
        ItemElement List *
        Vocation List *
        Map<string, WeaponSkillData> *
        WeaponSpell Set *
        DicePoolCalculationData

let init () : CombatRoll list = []

let update msg : CombatRoll list =
    match msg with
    | RecalculateCombatRollList(equipmentList,
                                weaponSkillList,
                                weaponSkillDataMap,
                                weaponSpellSet,
                                dicePoolCalculationData) ->
        createCombatRolls equipmentList weaponSkillList weaponSkillDataMap weaponSpellSet dicePoolCalculationData

open Feliz
open Feliz.Bulma
open FogentRoleplayLib.DicePoolMod

let combatRollRow (model: CombatRoll) =
    [
        model.itemName
        model.weaponTypeName
        $"{model.handedVariation} ({dicePoolModToString model.weaponandOffhandDicePoolModString})"
        $"{model.resourceName} ({dicePoolModToString model.resourceDicePoolMod})"
        (dicePoolToString model.dicePool)
        (string model.penetration)
        $"{calculatedRangeToString model.calculatedRange} ({model.calculatedRange.name})"
        (stringSetToStringSeperatedByCommas model.damageTypeSet)
        (match model.eoName with
         | Some eoName -> eoName
         | None -> ""
         |> sprintf "%d (%s)" model.calculatedEngageableOpponents)
        (setAreaOfEffectOptionToString model.setAreaOfEffectOption)
    ]
    |> List.map Html.td
    |> Html.tr

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
                        "Resource"
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