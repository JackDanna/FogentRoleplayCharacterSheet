module Effect

open FogentRoleplayLib.Effect

type Msg =
    | TextEffectMsg of InteractiveTextEffect.Msg
    | AttributeDeterminedDiceModMsg of PartiallyInteractiveTextEffect.Msg
    | SkillDiceModMsg of PartiallyInteractiveTextEffect.Msg

let update msg (model: Effect) : Effect =
    match msg, model with
    | TextEffectMsg msg, TextEffect textEffect -> InteractiveTextEffect.update msg textEffect |> TextEffect
    | AttributeDeterminedDiceModMsg msg, AttributeDeterminedDiceMod addm ->
        addm
        |> AttributeDeterminedDiceMod
        |> effectToTextEffectWithBlankDurationAndSource
        |> PartiallyInteractiveTextEffect.update msg
        |> (fun textEffect -> {
            addm with
                durationAndSource = textEffect.durationAndSource
        })
        |> AttributeDeterminedDiceMod
    | SkillDiceModMsg msg, SkillDiceMod sdm ->
        sdm
        |> SkillDiceMod
        |> effectToTextEffectWithBlankDurationAndSource
        |> (fun textEffect -> {
            sdm with
                durationAndSource = textEffect.durationAndSource
        })
        |> SkillDiceMod
    | _, _ -> model

open Feliz

let view model dispatch =
    match model with
    | TextEffect textEffect -> InteractiveTextEffect.view textEffect (TextEffectMsg >> dispatch)
    | AttributeDeterminedDiceMod addm ->
        PartiallyInteractiveTextEffect.view
            (effectToTextEffectWithBlankDurationAndSource (AttributeDeterminedDiceMod addm))
            (AttributeDeterminedDiceModMsg >> dispatch)
    | SkillDiceMod sdm ->
        PartiallyInteractiveTextEffect.view
            (effectToTextEffectWithBlankDurationAndSource (SkillDiceMod sdm))
            (SkillDiceModMsg >> dispatch)
    |> List.map Html.td