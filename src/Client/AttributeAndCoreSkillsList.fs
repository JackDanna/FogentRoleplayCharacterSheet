module AttributeAndCoreSkillsList

open FogentRoleplayLib.AttributeAndCoreSkills
open FogentRoleplayLib.AttributeAndCoreSkillsData
open FogentRoleplayLib.Effect
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | ModifyAttributeAndCoreSkillsList of int * AttributeAndCoreSkills.Msg
    | CalculateDicePools of DicePoolCalculationData

let init (effects: Effect List) attributeAndCoreSkillsDataSet =
    let initAttributeSet =
        Set.map
            (fun attributeAndCoreSkillsData -> AttributeStat.init attributeAndCoreSkillsData.governingAttributeName)
            attributeAndCoreSkillsDataSet

    let dicePoolCalculationData = {
        attributes = initAttributeSet
        effects = effects
    }

    attributeAndCoreSkillsDataSet
    |> Set.map (fun attributeAndCoreSkillsData ->
        AttributeAndCoreSkills.init attributeAndCoreSkillsData dicePoolCalculationData)

let update msg model =
    match msg with
    | ModifyAttributeAndCoreSkillsList(position, msg) ->
        model
        |> Set.toList
        |> List.mapi (fun index attributeAndCoreSkills ->
            if index = position then
                AttributeAndCoreSkills.update msg attributeAndCoreSkills
            else
                attributeAndCoreSkills)
        |> Set.ofList
    | CalculateDicePools dicePoolCalculationData ->
        model
        |> Set.toList
        |> List.map (fun attributeAndCoreSkills ->
            AttributeAndCoreSkills.update
                (AttributeAndCoreSkills.Msg.CalculateDicePool(dicePoolCalculationData))
                attributeAndCoreSkills)
        |> Set.ofList



open Feliz
open Feliz.Bulma

let view (model: AttributeAndCoreSkills Set) dispatch =
    Bulma.container [
        Bulma.label "Attributes and Core Skills:" |> Bulma.content
        Bulma.columns [
            columns.isCentered
            prop.children [
                model
                |> Set.toList
                |> List.mapi (fun position attributeAndCoreSkills ->
                    Bulma.column [
                        AttributeAndCoreSkills.view attributeAndCoreSkills (fun msg ->
                            ModifyAttributeAndCoreSkillsList(position, msg) |> dispatch)
                    ])
                |> Bulma.columns
            ]
        ]
    ]