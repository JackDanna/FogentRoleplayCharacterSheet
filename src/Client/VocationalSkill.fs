module VocationalSkill

open FogentRoleplayLib.VocationalSkill
open FogentRoleplayLib.Skill
open FogentRoleplayLib.StringUtils
open FogentRoleplayLib.AttributeName

type Msg =
    | SkillMsg of Skill.Msg
    | CalculateDicePool of DicePoolCalculationData
    | ToggleGoverningAttributes of AttributeName

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
    | ToggleGoverningAttributes newAttributeName -> {
        model with
            governingAttributeNames =
                model.governingAttributeNames
                |> Set.exists (fun attributeName -> attributeName = newAttributeName)
                |> (fun attributeNameExists ->
                    if attributeNameExists then
                        Set.add newAttributeName model.governingAttributeNames
                    else
                        Set.remove newAttributeName model.governingAttributeNames)
      }

open Feliz
open Feliz.Bulma


let governingAttributesToggle (model: VocationalSkill) dispatch (attributeNameSet: AttributeName Set) =
    Bulma.dropdown [
        dropdown.isHoverable
        prop.children [
            Bulma.dropdownTrigger [
                Bulma.button.button [ Html.span (stringSetToStringSeperatedByCommas model.governingAttributeNames) ]
            ]
            Bulma.dropdownMenu [

                List.map
                    (fun attributeName ->
                        Bulma.dropdownItem.a [
                            prop.onClick (fun _ -> dispatch (ToggleGoverningAttributes attributeName))
                            prop.children [
                                Bulma.columns [
                                    Bulma.column [
                                        Bulma.input.checkbox [
                                            prop.isChecked (
                                                List.contains attributeName (List.ofSeq model.governingAttributeNames)
                                            )
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

let view model dispatch canUserChangeLevel =

    Bulma.column [
        model.governingAttributeNames |> stringSetToStringSeperatedByCommas |> prop.text
    ]
    |> Some
    |> Skill.view model.skill (SkillMsg >> dispatch) canUserChangeLevel