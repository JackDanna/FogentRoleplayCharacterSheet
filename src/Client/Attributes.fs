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
open Feliz.DaisyUI
open ViewUtils

let attributesAndCoreSkillsListView model dispatch preloadedCoreSkillView =
    Html.div [
        Daisy.label [ prop.text "Attributes and Core Skills:" ]
        horizontalDiv [
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