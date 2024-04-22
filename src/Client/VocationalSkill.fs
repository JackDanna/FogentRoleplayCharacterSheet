module VocationalSkill

open FogentRoleplayLib.VocationalSkill
open FogentRoleplayLib.Skill
open FogentRoleplayLib.StringUtils
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.Neg1To5
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.ZeroToFive

type Msg =
    | SkillMsg of Skill.Msg
    | ToggleGoverningAttribute of AttributeName
    | SetSkillLevel of Neg1To5
    | CalculateDicePool of DicePoolCalculationData
    | CheckIfLevelCapExceeded of ZeroToFive

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
    | ToggleGoverningAttribute newAttributeName -> {
        model with
            governingAttributeNames = toggleAttributeNameSet model.governingAttributeNames newAttributeName
      }
    | SetSkillLevel newSkillLevel -> {
        model with
            skill.level = newSkillLevel
      }
    | CalculateDicePool msg -> {
        model with
            skill =
                Skill.update
                    (Skill.Msg.SetDicePoolModList(
                        calculateVocationalSkillDicePool msg model.skill.level model.governingAttributeNames
                    ))
                    model.skill
      }
    | CheckIfLevelCapExceeded levelCap ->
        if (model.skill.level |> neg1To5ToInt) > (levelCap |> zeroToFiveToUint |> int) then
            {
                model with
                    skill.level = levelCap |> ZeroToFiveToNeg1To5
            }
        else
            model


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

let view attributeNameSet model dispatch disableChangeLevel =

    Bulma.column [
        governingAttributesToggle model.governingAttributeNames (ToggleGoverningAttribute >> dispatch) attributeNameSet
    ]
    |> Some
    |> Skill.view model.skill (SkillMsg >> dispatch) disableChangeLevel