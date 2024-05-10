module VocationStat

open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.VocationStat

type Msg =
    | SetName of string
    | ZeroToFiveMsg of ZeroToFive.Msg
    | ToggleGoveringAttribute of AttributeName * option<DicePoolCalculationData>

let init () : VocationStat = {
    name = ""
    level = ZeroToFive.init ()
    governingAttributeNameSet = Set.empty
    baseDice = base3d6DicePool
    effectDicePoolModList = []
}

let update msg (model: VocationStat) =
    match msg with
    | SetName newName -> { model with name = newName }
    | ZeroToFiveMsg msg -> {
        model with
            level = ZeroToFive.update msg model.level
      }
    | ToggleGoveringAttribute(newAttributeName, dicePoolCalculationDataOption) ->
        match dicePoolCalculationDataOption with
        | Some dicePoolCalculationData ->
            let newGoverningAttributeNameSet =
                toggleAttributeNameSet model.governingAttributeNameSet newAttributeName

            {
                model with
                    governingAttributeNameSet = newGoverningAttributeNameSet
                    effectDicePoolModList =
                        determineEffectDicePoolModList dicePoolCalculationData model.governingAttributeNameSet
            }
        | None -> model


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
            VocationalSkill.governingAttributesToggle
                attributeNameSet
                model.governingAttributeNameSet
                (fun toggledAttributeName -> ToggleGoveringAttribute(toggledAttributeName, None) |> dispatch)
        ]
        Bulma.column [ ZeroToFive.view model.level (ZeroToFiveMsg >> dispatch) ]
        Bulma.column [
            prop.text (
                modifyDicePoolByDicePoolModList model.baseDice model.effectDicePoolModList
                |> dicePoolToString
            )
        ]
    ]