module AttributeAndCoreSkillsList

open FogentRoleplayLib.AttributeAndCoreSkills
open FogentRoleplayLib.Skill

type Msg =
    | ModifyAttributeAndCoreSkillsList of int * AttributeAndCoreSkills.Msg
    | CalculateDicePools of DicePoolCalculationData

let update msg model =
    match msg with
    | ModifyAttributeAndCoreSkillsList(position, msg) ->
        List.mapi
            (fun index attributeAndCoreSkills ->
                if index = position then
                    AttributeAndCoreSkills.update msg attributeAndCoreSkills
                else
                    attributeAndCoreSkills)
            model
    | CalculateDicePools dicePoolCalculationData ->

        List.map
            (fun attributeAndCoreSkills ->
                AttributeAndCoreSkills.update
                    (AttributeAndCoreSkills.Msg.CalculateDicePool(dicePoolCalculationData))
                    attributeAndCoreSkills)
            model


open Feliz
open Feliz.Bulma

let view (model: AttributeAndCoreSkills list) dispatch =
    Bulma.container [
        Bulma.label "Attributes and Core Skills:" |> Bulma.content
        Bulma.columns [
            columns.isCentered
            prop.children [
                List.mapi
                    (fun position attributeAndCoreSkills ->
                        Bulma.column [
                            Bulma.box [

                                AttributeAndCoreSkills.view attributeAndCoreSkills (fun msg ->
                                    ModifyAttributeAndCoreSkillsList(position, msg) |> dispatch)
                            ]
                        ])
                    model
                |> Bulma.columns
            ]
        ]
    ]