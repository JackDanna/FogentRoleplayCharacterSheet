module Item

open FogentRoleplayLib.Item
open FogentRoleplayLib.Effect

open Feliz
open Feliz.Bulma

let view (model: Item) =

    [
        Html.td model.name
        Html.td (effectsToCommaSeperatedEffectNames model.itemEffectSet)
        Html.td $"{model.itemTier.name} ({model.itemTier.level})"
        Html.td model.weight
        Html.td model.value
    ]