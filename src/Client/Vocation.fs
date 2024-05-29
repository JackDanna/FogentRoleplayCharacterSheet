module Vocation

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | VocationStatMsg of VocationStat.Msg
    | MundaneOrMagicVocationExtrasMsg of MundaneOrMagicVocationExtras.Msg
    | CalculateDicePools of DicePoolCalculationData

let init vocationName coreSkillMap dicePoolCalculationData magicSystemMap : Vocation =

    let vocationStat = VocationStat.init vocationName Set.empty dicePoolCalculationData

    {
        vocationStat = vocationStat
        mundaneOrMagicVocationExtras = MundaneOrMagicVocationExtras.init vocationStat coreSkillMap magicSystemMap
    }

let update (msg: Msg) (model: Vocation) =

    match msg with
    | VocationStatMsg msg -> {
        model with
            vocationStat = VocationStat.update msg model.vocationStat
      }
    | MundaneOrMagicVocationExtrasMsg msg -> {
        model with
            mundaneOrMagicVocationExtras = MundaneOrMagicVocationExtras.update msg model.mundaneOrMagicVocationExtras
      }
    | CalculateDicePools dicePoolCalculationData -> {
        vocationStat = VocationStat.update (VocationStat.CalculateDicePool dicePoolCalculationData) model.vocationStat
        mundaneOrMagicVocationExtras =
            MundaneOrMagicVocationExtras.update
                (MundaneOrMagicVocationExtras.CalculateDicePools dicePoolCalculationData)
                model.mundaneOrMagicVocationExtras
      }

open Feliz
open Feliz.Bulma

let view attributeNameSet (weaponSkillNameSet) (model: Vocation) dispatch =

    [
        VocationStat.view attributeNameSet model.vocationStat (VocationStatMsg >> dispatch)

    ]
    @ MundaneOrMagicVocationExtras.view
        attributeNameSet
        weaponSkillNameSet
        model.mundaneOrMagicVocationExtras
        (MundaneOrMagicVocationExtrasMsg >> dispatch)
    |> Bulma.box