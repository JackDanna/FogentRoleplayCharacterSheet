module MundaneOrMagicVocationExtras

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.MundaneOrMagicVocationExtras
open FogentRoleplayLib.VocationStat

type Msg =
    | MundaneVocationSkillsMsg of MundaneVocationSkills.Msg
    | MagicVocationExtrasMsg of MagicVocationExtras.Msg
    | CalculateDicePools of DicePoolCalculationData
    | RecalculateVocationResourcePool of MagicVocationExtras.RecalculateVocationResourcePoolMsg

let init vocationStat coreSkillMap (magicSystemMap: Map<string, MagicSystem>) =

    match magicSystemMap.TryFind vocationStat.name with
    | Some magicSystem ->
        MagicVocationExtras.init coreSkillMap magicSystem vocationStat.level vocationStat.dicePool
        |> MagicVocationExtras

    | None -> MundaneVocationSkills.init () |> MundaneVocationExtras

let update msg (model: MundaneOrMagicVocationExtras) =

    match msg, model with
    | MundaneVocationSkillsMsg msg, MundaneVocationExtras mundaneVocation ->
        MundaneVocationSkills.update msg mundaneVocation |> MundaneVocationExtras

    | MagicVocationExtrasMsg msg, MagicVocationExtras magicVocation ->
        MagicVocationExtras.update msg magicVocation |> MagicVocationExtras

    | CalculateDicePools dicePoolCalculationData, MundaneVocationExtras mundaneVocation ->
        MundaneVocationSkills.update (MundaneVocationSkills.CalculateDicePools dicePoolCalculationData) mundaneVocation
        |> MundaneVocationExtras

    | CalculateDicePools dicePoolCalculationData, MagicVocationExtras magicVocation ->
        MagicVocationExtras.update
            (MagicVocationExtras.CalculateMagicVocationSkillDicePools dicePoolCalculationData)
            magicVocation
        |> MagicVocationExtras

    | RecalculateVocationResourcePool msg, MagicVocationExtras magicVocation ->
        MagicVocationExtras.update (MagicVocationExtras.RecalculateVocationResourcePool msg) magicVocation
        |> MagicVocationExtras

    | _, _ -> model

open Feliz
open Feliz.Bulma

let view attributeNameSet (weaponSkillNameSet) (model: MundaneOrMagicVocationExtras) dispatch =

    match model with
    | MundaneVocationExtras mundaneVocation ->
        MundaneVocationSkills.view
            attributeNameSet
            weaponSkillNameSet
            mundaneVocation
            (MundaneVocationSkillsMsg >> dispatch)
    | MagicVocationExtras magicVocationExtras ->
        MagicVocationExtras.view
            attributeNameSet
            weaponSkillNameSet
            magicVocationExtras
            (MagicVocationExtrasMsg >> dispatch)