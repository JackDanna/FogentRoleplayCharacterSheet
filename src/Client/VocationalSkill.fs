module VocationalSkill

open FogentRoleplayLib.VocationalSkill
open FogentRoleplayLib.Skill
open FogentRoleplayLib.StringUtils
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.Neg1To5

type Msg =
    | SkillMsg of Skill.Msg
    | CalculateDicePool of DicePoolCalculationData
    | ToggleGoverningAttribute of AttributeName
    | SetSkillLevel of Neg1To5

let init () = {
    skill = Skill.init ()
    governingAttributeNames = Set.empty
}

let update msg model =
    match msg with
    | SkillMsg msg -> {
        model with
            skill = Skill.update msg model.skill
      }
    | CalculateDicePool msg -> {
        model with
            skill =
                Skill.update
                    (Skill.Msg.SetDicePool(
                        calculateVocationalSkillDicePool msg model.skill.level model.governingAttributeNames
                    ))
                    model.skill
      }
    | ToggleGoverningAttribute newAttributeName -> {
        model with
            governingAttributeNames = toggleAttributeNameSet model.governingAttributeNames newAttributeName
      }
    | SetSkillLevel newSkillLevel -> {
        model with
            skill.level = newSkillLevel
      }

open Feliz
open Feliz.Bulma


let governingAttributesToggle
    (model: AttributeName Set)
    dispatchToggleGoverningAttribute
    (attributeNameSet: AttributeName Set)
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

let view attributeNameSet model dispatch canUserChangeLevel =

    Bulma.column [
        governingAttributesToggle model.governingAttributeNames (ToggleGoverningAttribute >> dispatch) attributeNameSet
    ]
    |> Some
    |> Skill.view model.skill (SkillMsg >> dispatch) true canUserChangeLevel