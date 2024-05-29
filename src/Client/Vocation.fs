module Vocation

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.MundaneOrMagicVocationExtras

type Msg =
    | VocationStatMsg of VocationStat.Msg
    | MundaneOrMagicVocationExtrasMsg of MundaneOrMagicVocationExtras.Msg
    | CalculateDicePools of DicePoolCalculationData

let init vocationName coreSkillMap dicePoolCalculationData (magicSystemMap: Map<string, MagicSystem>) : Vocation =
    let vocationStat =
        match magicSystemMap.TryFind vocationName with
        | Some magicSystem ->
            VocationStat.init vocationName magicSystem.vocationGoverningAttributeSet dicePoolCalculationData

        | None -> VocationStat.init vocationName Set.empty dicePoolCalculationData

    {
        vocationStat = vocationStat
        mundaneOrMagicVocationExtras = MundaneOrMagicVocationExtras.init vocationStat coreSkillMap magicSystemMap
    }

let update (msg: Msg) (model: Vocation) =

    match msg with
    // Checks for ToggleGoverningAttribute
    | VocationStatMsg(VocationStat.ToggleGoveringAttribute(attributeName, dicePoolCalculationDataOption)) ->
        let vocationStat =
            VocationStat.update
                (VocationStat.ToggleGoveringAttribute(attributeName, dicePoolCalculationDataOption))
                model.vocationStat

        {
            model with
                vocationStat = vocationStat
                mundaneOrMagicVocationExtras =
                    MundaneOrMagicVocationExtras.update
                        (MundaneOrMagicVocationExtras.RecalculateVocationResourcePool(
                            vocationStat.level,
                            vocationStat.dicePool
                        ))
                        model.mundaneOrMagicVocationExtras
        }
    | VocationStatMsg(VocationStat.Msg.ZeroToFiveMsg(msg, dicePoolCalculationData)) ->
        let newVocationStat =
            VocationStat.update (VocationStat.Msg.ZeroToFiveMsg(msg, dicePoolCalculationData)) model.vocationStat

        {
            vocationStat = newVocationStat
            mundaneOrMagicVocationExtras =
                MundaneOrMagicVocationExtras.update
                    (MundaneOrMagicVocationExtras.RecalculateVocationResourcePool(
                        newVocationStat.level,
                        newVocationStat.dicePool
                    ))
                    model.mundaneOrMagicVocationExtras
        }
    | VocationStatMsg msg -> {
        model with
            vocationStat = VocationStat.update msg model.vocationStat
      }
    | MundaneOrMagicVocationExtrasMsg msg -> {
        model with
            mundaneOrMagicVocationExtras = MundaneOrMagicVocationExtras.update msg model.mundaneOrMagicVocationExtras
      }
    | CalculateDicePools dicePoolCalculationData ->
        let newVocationStat =
            VocationStat.update (VocationStat.CalculateDicePool dicePoolCalculationData) model.vocationStat

        {
            vocationStat = newVocationStat
            mundaneOrMagicVocationExtras =
                model.mundaneOrMagicVocationExtras
                |> MundaneOrMagicVocationExtras.update (
                    MundaneOrMagicVocationExtras.CalculateDicePools dicePoolCalculationData
                )
                |> MundaneOrMagicVocationExtras.update (
                    MundaneOrMagicVocationExtras.RecalculateVocationResourcePool(
                        newVocationStat.level,
                        newVocationStat.dicePool
                    )
                )
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