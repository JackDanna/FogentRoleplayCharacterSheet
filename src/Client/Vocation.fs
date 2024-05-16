module Vocation

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | MundaneVocationMsg of MundaneVocation.Msg
    | MagicVocationMsg of MagicVocation.Msg
    | CalculateDicePools of DicePoolCalculationData

let init name dicePoolCalculationData : Vocation =
    MundaneVocation.init name dicePoolCalculationData |> MundaneVocation

let update msg (model: Vocation) =

    match msg, model with
    | MundaneVocationMsg msg, MundaneVocation mundaneVocation ->
        MundaneVocation.update msg mundaneVocation |> MundaneVocation
    | MagicVocationMsg msg, MagicVocation magicVocation -> MagicVocation.update msg magicVocation |> MagicVocation
    | CalculateDicePools dicePoolCalculationData, vocation ->
        match vocation with
        | MundaneVocation mundaneVocation ->
            MundaneVocation.update (MundaneVocation.CalculateDicePools dicePoolCalculationData) mundaneVocation
            |> MundaneVocation
        | MagicVocation magicVocation ->
            MagicVocation.update
                (MagicVocation.CalculateMagicVocationSkillDicePools dicePoolCalculationData)
                magicVocation
            |> MagicVocation

    | _, _ -> model


open Feliz
open Feliz.Bulma

let view attributeNameSet (weaponSkillNameSet) (model: Vocation) dispatch =
    match model with
    | MundaneVocation mundaneVocation ->
        MundaneVocation.view attributeNameSet weaponSkillNameSet mundaneVocation (MundaneVocationMsg >> dispatch)
    | MagicVocation magicVocation ->
        MagicVocation.view attributeNameSet weaponSkillNameSet magicVocation (MagicVocationMsg >> dispatch)
    |> Bulma.box