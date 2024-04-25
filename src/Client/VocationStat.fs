module VocationStat

open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.VocationStat

type Msg =
    | SetName of string
    | ZeroToFiveMsg of ZeroToFive.Msg
    | ToggleGoveringAttribute of AttributeName
    | CalculateDicePool of DicePoolCalculationData

let init () : VocationStat = {
    name = ""
    level = ZeroToFive.init ()
    governingAttributeNameSet = Set.empty
    baseDice = base3d6DicePool
    dicePoolModList = []
}

let update msg (model: VocationStat) =
    match msg with
    | SetName newName -> { model with name = newName }
    | ZeroToFiveMsg msg -> {
        model with
            level = ZeroToFive.update msg model.level
      }
    | ToggleGoveringAttribute newAttributeName -> {
        model with
            governingAttributeNameSet = toggleAttributeNameSet model.governingAttributeNameSet newAttributeName
      }
    | CalculateDicePool msg -> {
        model with
            dicePoolModList = calculateVocationStatDicePoolModList msg model.level model.governingAttributeNameSet
      }


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
                model.governingAttributeNameSet
                (ToggleGoveringAttribute >> dispatch)
                attributeNameSet
        ]
        Bulma.column [ ZeroToFive.view model.level (ZeroToFiveMsg >> dispatch) ]
        Bulma.column [
            prop.text (
                modifyDicePoolByDicePoolModList model.baseDice model.dicePoolModList
                |> dicePoolToString
            )
        ]
    ]