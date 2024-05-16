module VocationalSkill

open FogentRoleplayLib.VocationalSkill
open FogentRoleplayLib.StringUtils
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.ZeroToFive

type Msg =
    | SkillMsg of Skill.Msg
    | ToggleGoverningAttribute of AttributeName * option<DicePoolCalculationData>
    | CalculateVocationalSkillDicePool of DicePoolCalculationData
    | CheckIfLevelCapExceeded of ZeroToFive * DicePoolCalculationData

let init governingAttributeSet dicePoolCalculationData skillName =

    {
        skill = Skill.init skillName governingAttributeSet dicePoolCalculationData
        governingAttributeNames = governingAttributeSet
    }

let update msg model =

    match msg with
    | SkillMsg msg -> {
        model with
            skill = Skill.update msg model.skill
      }

    | ToggleGoverningAttribute(newAttributeName, Some dicePoolCalculationData) ->
        let newGoverningAttribteNameSet =
            toggleAttributeNameSet model.governingAttributeNames newAttributeName

        {
            model with
                governingAttributeNames = newGoverningAttribteNameSet
                skill =
                    Skill.update
                        (Skill.Msg.CalculateDicePool(newGoverningAttribteNameSet, dicePoolCalculationData))
                        model.skill
        }

    | CalculateVocationalSkillDicePool dicePoolCalculationData -> {
        model with
            skill =
                Skill.update
                    (Skill.Msg.CalculateDicePool(model.governingAttributeNames, dicePoolCalculationData))
                    model.skill
      }

    | CheckIfLevelCapExceeded(levelCap, dicePoolCalculationData) -> {
        model with
            skill =
                Skill.update
                    (Skill.Msg.CheckIfLevelCapExceeded(levelCap, model.governingAttributeNames, dicePoolCalculationData))
                    model.skill
      }
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

let view attributeNameSet model dispatch disableChangeLevel =

    Bulma.column [
        governingAttributesToggle attributeNameSet model.governingAttributeNames (fun toggledAttributeName ->
            ToggleGoverningAttribute(toggledAttributeName, None) |> dispatch)
    ]
    |> Some
    |> Skill.view model.skill (SkillMsg >> dispatch) disableChangeLevel