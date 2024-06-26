module VocationStat

open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.VocationStat
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | SetName of string
    | ZeroToFiveMsg of ZeroToFive.Msg * option<DicePoolCalculationData>
    | ToggleGoveringAttribute of AttributeName * option<DicePoolCalculationData>
    | CalculateDicePool of DicePoolCalculationData

let init name governingAttributeNameSet dicePoolCalculationData : VocationStat =
    let level = ZeroToFive.init ()

    {
        name = name
        level = level
        governingAttributeNameSet = governingAttributeNameSet
        dicePool = calculateVocationStatDicePool name level governingAttributeNameSet dicePoolCalculationData
    }

let update msg (model: VocationStat) =
    let temp dicePoolCalculationData =
        calculateVocationStatDicePool model.name model.level model.governingAttributeNameSet dicePoolCalculationData

    match msg with
    | SetName newName -> { model with name = newName }
    | ZeroToFiveMsg(msg, Some dicePoolCalculationData) ->
        let newLevel = ZeroToFive.update msg model.level

        {
            model with
                level = newLevel
                dicePool =
                    calculateVocationStatDicePool
                        model.name
                        newLevel
                        model.governingAttributeNameSet
                        dicePoolCalculationData
        }
    | ToggleGoveringAttribute(newAttributeName, Some dicePoolCalculationData) ->
        let newGoverningAttributeNameSet =
            toggleAttributeNameSet model.governingAttributeNameSet newAttributeName

        {
            model with
                governingAttributeNameSet = newGoverningAttributeNameSet
                dicePool =
                    calculateVocationStatDicePool
                        model.name
                        model.level
                        newGoverningAttributeNameSet
                        dicePoolCalculationData
        }
    | CalculateDicePool dicePoolCalculationData -> {
        model with
            dicePool = temp dicePoolCalculationData
      }
    | _ -> model


open Feliz
open Feliz.Bulma

let view attributeNameSet (model: VocationStat) dispatch =
    Bulma.columns [
        Bulma.column [
            Bulma.input.text [
                prop.value model.name
                prop.onTextChange (fun value -> dispatch (SetName value))
            ]
        ]
        Bulma.column [
            Skill.governingAttributesToggle
                attributeNameSet
                model.governingAttributeNameSet
                (fun toggledAttributeName -> ToggleGoveringAttribute(toggledAttributeName, None) |> dispatch)
        ]
        Bulma.column [
            ZeroToFive.view model.level ((fun msg -> ZeroToFiveMsg(msg, None)) >> dispatch)
        ]
        Bulma.column [ prop.text (model.dicePool |> dicePoolToString) ]
    ]