module CombatRollList

open FogentRoleplayLib.CombatRoll

open FogentRoleplayLib.DicePool
open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.Range
open FogentRoleplayLib.SetAreaOfEffect
open FogentRoleplayLib.ItemStack
open FogentRoleplayLib.VocationSkill

open FogentRoleplayLib.StringUtils



type Msg = RecalculateCombatRollList of ItemStack List * VocationSkill List

let init () : CombatRoll list = []

let update msg : CombatRoll list =
    match msg with
    | RecalculateCombatRollList(equipmentList, vocationSkillList) ->
        createWeaponItemCombatRolls equipmentList vocationSkillList

open Feliz
open Feliz.Bulma

let combatRollRow (model: CombatRoll) =
    Html.tr [
        Html.td model.name
        Html.td (model.dicePool |> dicePoolToString)
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