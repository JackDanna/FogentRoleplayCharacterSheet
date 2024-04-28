module Vocation

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.Character

open FogentRoleplayLib.MundaneVocation
open FogentRoleplayLib.MagicVocation

type Msg =
    | MundaneVocationMsg of MundaneVocation.Msg
    | MagicVocationMsg of MagicVocation.Msg
    | CalculateDicePools of DicePoolCalculationData

let init () : Vocation =
    MundaneVocation.init () |> MundaneVocation

let update msg (model: Vocation) =
    match msg, model with
    | MundaneVocationMsg msg, MundaneVocation mundaneVocation ->
        MundaneVocation.update msg mundaneVocation |> MundaneVocation
    | MagicVocationMsg msg, MagicVocation magicVocation -> MagicVocation.update msg magicVocation |> MagicVocation
    | CalculateDicePools msg, MundaneVocation mundaneVocation ->
        MundaneVocation.update (MundaneVocation.CalculateDicePools msg) mundaneVocation
        |> MundaneVocation
    | _, _ -> model


open Feliz
open Feliz.Bulma

let view attributeNameSet (vocationSkillData: VocationSkillData) (model: Vocation) dispatch =
    match model with
    | MundaneVocation mundaneVocation ->
        MundaneVocation.view
            attributeNameSet
            vocationSkillData.weaponGoverningSkillNameSet
            mundaneVocation
            (MundaneVocationMsg >> dispatch)
    | MagicVocation magicVocation ->
        MagicVocation.view
            attributeNameSet
            vocationSkillData.weaponGoverningSkillNameSet
            magicVocation
            (MagicVocationMsg >> dispatch)
    |> Bulma.box