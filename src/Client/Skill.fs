module Skill

open FogentRoleplayLib.Skill
open FogentRoleplayLib.DicePool
open FogentRoleplayLib.ZeroToFive
open FogentRoleplayLib.Neg1To5
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.StringUtils
open FogentRoleplayLib.CoreSkillData

type ZeroToFiveAndDicePoolCalculationData = ZeroToFive * DicePoolCalculationData

type Msg =
    | ModifySkillLevel of Neg1To5.Msg * option<ZeroToFive> * option<DicePoolCalculationData>
    | CalculateDicePool of DicePoolCalculationData
    | CheckIfLevelCapExceeded of ZeroToFiveAndDicePoolCalculationData
    | ToggleGoverningAttribute of AttributeName * option<DicePoolCalculationData>
    | ModifySkillLevelWithVocationStatLevel of ZeroToFiveAndDicePoolCalculationData
    | NoOp

let init = FogentRoleplayLib.Skill.init

let initCoreSkill coreSkillData dicePoolCalculationData =
    FogentRoleplayLib.Skill.init
        coreSkillData.skillName
        (Set.ofList [ coreSkillData.attributeName ])
        dicePoolCalculationData

let update msg (model: Skill) =

    match msg with
    | ModifySkillLevel(neg1To5Msg, levelCapOption, Some dicePoolCalculationData) ->
        let msgLevel = Neg1To5.update neg1To5Msg model.level

        let newLevel =
            match levelCapOption with
            | Some levelCap ->
                if (zeroToFiveToInt levelCap) < (neg1To5ToInt msgLevel) then
                    model.level
                else
                    msgLevel
            | None -> msgLevel

        {
            model with
                level = newLevel
                dicePool =
                    calculateSkillDicePool model.name newLevel model.governingAttributeNames dicePoolCalculationData
        }
    | CalculateDicePool dicePoolCalculationData ->

        {
            model with
                dicePool =
                    calculateSkillDicePool model.name model.level model.governingAttributeNames dicePoolCalculationData
        }
    | CheckIfLevelCapExceeded(levelCap, dicePoolCalculationData) ->
        if (zeroToFiveToInt levelCap) < (neg1To5ToInt model.level) then

            let convertedLevelCap = levelCap |> zeroToFiveToNeg1To5

            {
                model with
                    level = convertedLevelCap
                    dicePool =
                        calculateSkillDicePool
                            model.name
                            convertedLevelCap
                            model.governingAttributeNames
                            dicePoolCalculationData
            }
        else
            model

    | ToggleGoverningAttribute(newAttributeName, Some dicePoolCalculationData) ->
        let newGoverningAttribteNameSet =
            toggleAttributeNameSet model.governingAttributeNames newAttributeName

        {
            model with
                governingAttributeNames = newGoverningAttribteNameSet
                dicePool =
                    calculateSkillDicePool model.name model.level newGoverningAttribteNameSet dicePoolCalculationData
        }
    | ModifySkillLevelWithVocationStatLevel(vocationStatLevel, dicePoolCalculationData) ->
        let newLevel = zeroToFiveToNeg1To5 vocationStatLevel

        {
            model with
                level = newLevel
                dicePool =
                    calculateSkillDicePool model.name newLevel model.governingAttributeNames dicePoolCalculationData
        }

    | NoOp
    | _ -> model



open Feliz
open Feliz.Bulma

let governingAttributesToggle
    (attributeNameSet: AttributeName Set)
    (model: AttributeName Set)
    dispatchToggleGoverningAttribute
    =
    Bulma.dropdown [
        dropdown.isHoverable
        prop.children [
            Bulma.dropdownTrigger [ Bulma.button.button [ Html.span (stringSetToStringSeperatedByCommas model) ] ]
            Bulma.dropdownMenu [

                List.map
                    (fun attributeName ->
                        Bulma.dropdownItem.a [
                            prop.onClick (fun _ -> dispatchToggleGoverningAttribute attributeName)
                            prop.children [
                                Bulma.columns [
                                    Bulma.column [
                                        Bulma.input.checkbox [
                                            prop.isChecked (List.contains attributeName (List.ofSeq model))
                                        ]
                                    ]
                                    Bulma.column [ prop.text attributeName ]
                                ]
                            ]
                        ])
                    (attributeNameSet |> List.ofSeq)
                |> Bulma.dropdownContent
            ]
        ]
    ]

let viewAsList attributeNameSet (model: Skill) dispatch userInputDisabled showGoverningSkillColumn = [
    Bulma.column [ prop.text model.name ]
    if showGoverningSkillColumn then
        Bulma.column [
            governingAttributesToggle attributeNameSet model.governingAttributeNames (fun toggledAttributeName ->
                ToggleGoverningAttribute(toggledAttributeName, None) |> dispatch)
        ]
    else
        Html.none
    Bulma.column [
        Neg1To5.view model.level ((fun msg -> ModifySkillLevel(msg, None, None)) >> dispatch) userInputDisabled
    ]
    Bulma.column [ model.dicePool |> dicePoolToString |> prop.text ]
]

let view attributeNameSet (model: Skill) dispatch disableChangeLevel showGoverningSkillColumn =
    viewAsList attributeNameSet model dispatch disableChangeLevel showGoverningSkillColumn
    |> Bulma.columns

let coreSkillView model dispatch =
    view Set.empty model dispatch false false