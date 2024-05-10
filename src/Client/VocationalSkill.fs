module VocationalSkill

open FogentRoleplayLib.VocationalSkill
open FogentRoleplayLib.StringUtils
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | SkillMsg of Skill.Msg
    | ToggleGoverningAttribute of AttributeName * option<DicePoolCalculationData>
    | SetEffectDicePoolModList of DicePoolCalculationData

let init dicePoolCalculationData skillName = {
    skill =
        Skill.init
            skillName
            dicePoolCalculationData.baseDiceEffectList
            (determineEffectDicePoolModList dicePoolCalculationData Set.empty)
    governingAttributeNames = Set.empty
}

let update msg model =
    let temp dicePoolCalculationData =
        Skill.update
            (Skill.Msg.SetEffectDicePoolModList(
                determineEffectDicePoolModList dicePoolCalculationData model.governingAttributeNames
            ))
            model.skill

    match msg with
    | SkillMsg msg -> {
        model with
            skill = Skill.update msg model.skill
      }
    | ToggleGoverningAttribute(newAttributeName, dicePoolCalculationDataOption) ->
        match dicePoolCalculationDataOption with
        | Some dicePoolCalculationData ->

            let newGoverningAttribteNameSet =
                toggleAttributeNameSet model.governingAttributeNames newAttributeName

            {
                model with
                    governingAttributeNames = newGoverningAttribteNameSet
                    skill = temp dicePoolCalculationData
            }
        | None -> model

    | SetEffectDicePoolModList msg -> { model with skill = temp msg }

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