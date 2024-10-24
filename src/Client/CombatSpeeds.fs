module CombatSpeeds

open FogentRoleplayLib.CombatSpeed
open FogentRoleplayLib.CombatSpeedCalculation
open FogentRoleplayLib.Skill
open FogentRoleplayLib.Attribute

type Msg =
    | RecalculateAllCombatSpeeds of Skill Set * Attribute Set
    | Insert of string * option<Skill Set> * option<Attribute Set> * option<Map<string, CombatSpeedCalculation>>
    | Remove of int

let init = FogentRoleplayLib.ListUtils.init

let update msg model =
    match msg with
    | RecalculateAllCombatSpeeds(skills, attributes) ->
        model
        |> List.map (fun combatSpeed ->
            CombatSpeed.update (CombatSpeed.RecalculateCombatSpeed(skills, attributes)) combatSpeed)
    | Insert(name, Some skills, Some attributes, Some combatSpeedCalculationMap) ->
        combatSpeedCalculationMap.Item name
        |> CombatSpeed.init skills attributes
        |> List.singleton
        |> List.append model
    | Remove pos -> List.removeAt pos model
    | _ -> model

open Feliz
open Feliz.DaisyUI

let view (combatSpeedCalculationNames: string Set) (model: CombatSpeed list) (dispatch: Msg -> unit) =
    Html.div [
        Daisy.labelText "Combat Speeds:"
        Daisy.table [
            Html.thead [
                [ "Name"; "Speed (ft)"; "Description" ]
                |> List.map (fun (thString: string) -> Html.th thString)
                |> Html.tr
            ]
            Html.tableBody (
                List.mapi
                    (fun position combatSpeed ->
                        let combatSpeed = CombatSpeed.view combatSpeed

                        let deleteEquipmentRowButton =
                            Html.td [
                                Html.button [ prop.onClick (fun _ -> dispatch (Remove(position))); prop.text "-" ]
                            ]
                            |> List.singleton

                        Html.tr (List.append combatSpeed deleteEquipmentRowButton))
                    model
            )
            Html.tfoot [
                ViewUtils.textInputWithDropdownSet
                    (fun input -> dispatch (Insert(input, None, None, None)))
                    combatSpeedCalculationNames
                    "CombatSpeeds"
            ]
        ]
    ]