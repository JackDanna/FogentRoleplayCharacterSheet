module Vocation

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.Character

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

    | CalculateDicePools msg, _ -> model
    // {
    //     model with
    //         vocationStat = VocationStat.update (VocationStat.CalculateDicePool msg) model.vocationStat
    //         vocationSkillList =
    //             MundaneVocationSkillList.update
    //                 (MundaneVocationSkillList.CalculateDicePools msg)
    //                 model.vocationSkillList
    // }
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
            vocationSkillData.magicSystemMap
            magicVocation
            (MagicVocationMsg >> dispatch)
    |> Bulma.box