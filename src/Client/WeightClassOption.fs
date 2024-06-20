module WeightClassOption

open FogentRoleplayLib.WeightClass
open FogentRoleplayLib.CarryWeightCalculation
open FogentRoleplayLib.ItemElement
open FogentRoleplayLib.Skill
open FogentRoleplayLib.Attribute
open FogentRoleplayLib.TextEffect

type Msg =
    | DetermineWeightClass of
        option<CarryWeightCalculation> *
        WeightClass Set *
        Attribute Set *
        Skill Set *
        ItemElement List

let init (carryWeightCalculationOption) weightClassSet attributes coreSkills equipment =
    match carryWeightCalculationOption with
    | Some carryWeightCalculation ->
        determineWeightClass
            (calculateCarryWeight (carryWeightCalculation) attributes coreSkills)
            (sumItemElementListWeight equipment)
            (weightClassSet)
    | None -> None

let update msg (model: WeightClass option) =
    match msg with
    | DetermineWeightClass(carryWeightCalculationOption, weightClassSet, attributes, coreSkills, equipment) ->
        init carryWeightCalculationOption weightClassSet attributes coreSkills equipment

open Feliz
open Feliz.Bulma

let view (model: WeightClass option) equipmentWeight maxWeight =
    match model with
    | Some weightClass ->
        weightClass.attributeDeterminedDiceModEffect
        |> (fun (attributeDeterminedDiceMod: FogentRoleplayLib.AttributeDeterminedDiceMod.AttributeDeterminedDiceMod) -> {
            attributeDeterminedDiceMod with
                durationAndSource = {
                    duration = "Indefinite"
                    source = weightClassOptionSourceString model equipmentWeight maxWeight
                }
        })
        |> attributeDeterminedDiceModToTextEffect
        |> NonInteractiveTextEffect.view
    | None -> []
    |> List.map Html.td