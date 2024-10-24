module Item

open FogentRoleplayLib.Item
open FogentRoleplayLib.Effect

open Feliz

let view (model: Item) =

    model.name, effectsToCommaSeperatedEffectNames model.itemEffectSet, model.weight, model.value