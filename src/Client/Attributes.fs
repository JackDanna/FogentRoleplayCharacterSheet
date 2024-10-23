module Attributes

type Msg = ModifyAttribute of int * Attribute.Msg

let init attributeNameSet =
    attributeNameSet |> Set.map (fun x -> Attribute.init x)

let update msg model =
    match msg with
    | ModifyAttribute(position, msg) ->
        model
        |> Set.toList
        |> List.mapi (fun index attribute ->
            if index = position then
                Attribute.update msg attribute
            else
                attribute)
        |> Set.ofList

open Feliz
open Feliz.Bulma

let attributesAndCoreSkillsListView model dispatch preloadedCoreSkillView =
    Bulma.container [
        Bulma.label "Attributes and Core Skills:" |> Bulma.content
        Bulma.columns [
            columns.isCentered
            prop.children (
                model
                |> Seq.mapi (fun position attribute ->
                    Attribute.attributeAndCoreSkills
                        attribute
                        (fun msg -> ModifyAttribute(position, msg) |> dispatch)
                        preloadedCoreSkillView)
            )
        ]
    ]