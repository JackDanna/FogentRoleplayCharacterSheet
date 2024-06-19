module WeightClassOption

open FogentRoleplayLib.SettingData
open FogentRoleplayLib.WeightClass
open FogentRoleplayLib.CarryWeightCalculation
open FogentRoleplayLib.ItemElement
open FogentRoleplayLib.Skill
open FogentRoleplayLib.Attribute
open FogentRoleplayLib.TextEffect

type Msg =
    | DetermineWeightClass of
        option<Map<string, CarryWeightCalculation> * WeightClass Set * Attribute Set * Skill Set * ItemElement List>

let init
    (carryWeightCalculationMap: Map<string, CarryWeightCalculation>)
    weightClassSet
    attributes
    coreSkills
    equipment
    =
    match carryWeightCalculationMap.TryFind "Carry Weight" with
    | Some carryWeightCalculation ->
        determineWeightClass
            (calculateCarryWeight (carryWeightCalculation) attributes coreSkills)
            (sumItemElementListWeight equipment)
            (weightClassSet)
    | None -> None

let update msg (model: WeightClass option) =
    match msg with
    | DetermineWeightClass(Some(carryWeightCalculationMap, weightClassSet, attributes, coreSkills, equipment)) ->
        init carryWeightCalculationMap weightClassSet attributes coreSkills equipment
    | _ -> model

open Feliz
open Feliz.Bulma

let view (model: WeightClass option) =
    match model with
    | Some weightClass ->
        weightClass.attributeDeterminedDiceModEffect
        |> (fun (attributeDeterminedDiceMod: FogentRoleplayLib.AttributeDeterminedDiceMod.AttributeDeterminedDiceMod) -> {
            attributeDeterminedDiceMod with
                durationAndSource = { duration = ""; source = "" }
        })
        |> attributeDeterminedDiceModToTextEffect
        |> NonInteractiveTextEffect.view
    | None -> [ Html.none ]
    |> Bulma.box