module Effect

open FogentRoleplayLib.Effect

type Msg =
    | TextEffectMsg of InteractiveTextEffect.Msg
    | AttributeDeterminedDiceModMsg of PartiallyInteractiveTextEffect.Msg
    | SkillDiceModMsg of PartiallyInteractiveTextEffect.Msg
    | PhysicalDefenseMsg of PartiallyInteractiveTextEffect.Msg

let update msg (model: Effect) : Effect =
    match msg, model with
    | TextEffectMsg msg, TextEffect textEffect -> InteractiveTextEffect.update msg textEffect |> TextEffect
    | AttributeDeterminedDiceModMsg msg, AttributeDeterminedDiceMod addm ->
        addm
        |> AttributeDeterminedDiceMod
        |> effectToTextEffect
        |> PartiallyInteractiveTextEffect.update msg
        |> (fun textEffect -> {
            addm with
                durationAndSource = textEffect.durationAndSource
        })
        |> AttributeDeterminedDiceMod
    | SkillDiceModMsg msg, SkillDiceMod sdm ->
        sdm
        |> SkillDiceMod
        |> effectToTextEffect
        |> (fun textEffect -> {
            sdm with
                durationAndSource = textEffect.durationAndSource
        })
        |> SkillDiceMod
    | PhysicalDefenseMsg msg, PhysicalDefense pd ->
        pd
        |> PhysicalDefense
        |> effectToTextEffect
        |> PartiallyInteractiveTextEffect.update msg
        |> (fun textEffect -> {
            pd with
                durationAndSource = textEffect.durationAndSource
        })
        |> PhysicalDefense
    | _, _ -> model

open Feliz

let view model dispatch =
    match model with
    | TextEffect textEffect -> InteractiveTextEffect.view textEffect (TextEffectMsg >> dispatch)
    | AttributeDeterminedDiceMod addm ->
        PartiallyInteractiveTextEffect.view
            (effectToTextEffect (AttributeDeterminedDiceMod addm))
            (AttributeDeterminedDiceModMsg >> dispatch)
    | SkillDiceMod sdm ->
        PartiallyInteractiveTextEffect.view (effectToTextEffect (SkillDiceMod sdm)) (SkillDiceModMsg >> dispatch)
    | PhysicalDefense pd ->
        PartiallyInteractiveTextEffect.view (effectToTextEffect (PhysicalDefense pd)) (PhysicalDefenseMsg >> dispatch)
    |> List.map Html.td