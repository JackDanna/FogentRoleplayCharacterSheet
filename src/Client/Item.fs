module Item

open FogentRoleplayLib.Item
open FogentRoleplayLib.Effect

open Feliz

let view (model: Item) =

    [
        Html.td model.name
        Html.td (effectsToCommaSeperatedEffectNames model.itemEffectSet)
        Html.td model.weight
        Html.td model.value
    ]