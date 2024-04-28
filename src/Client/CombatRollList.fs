module CombatRollList

open FogentRoleplayLib.CombatRoll

open FogentRoleplayLib.DicePool
open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.Range
open FogentRoleplayLib.SetAreaOfEffect
open FogentRoleplayLib.ItemStack
open FogentRoleplayLib.VocationalSkill

open FogentRoleplayLib.StringUtils



type Msg = RecalculateCombatRollList of ItemStack List * VocationalSkill List

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