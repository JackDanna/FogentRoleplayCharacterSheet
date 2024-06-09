module Vocation

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.MagicSystem

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

    let temp =
        (fun msg ->
            let newVocationStat = VocationStat.update msg model.vocationStat

            {
                vocationStat = newVocationStat
                mundaneOrMagicVocationExtras =
                    MundaneOrMagicVocationExtras.update
                        (MundaneOrMagicVocationExtras.RecalculateVocationResourcePool(
                            newVocationStat.level,
                            newVocationStat.dicePool
                        ))
                        model.mundaneOrMagicVocationExtras
            })

    match msg with
    // Checks for ToggleGoverningAttribute
    | VocationStatMsg(msg) ->
        match msg with
        | VocationStat.ToggleGoveringAttribute(attributeName, dicePoolCalculationDataOption) ->
            (VocationStat.ToggleGoveringAttribute(attributeName, dicePoolCalculationDataOption))
            |> temp

        | VocationStat.Msg.ZeroToFiveMsg(msg, Some dicePoolCalculationData) ->
            (VocationStat.Msg.ZeroToFiveMsg(msg, Some dicePoolCalculationData))
            |> temp
            |> (fun vocation -> {
                vocation with
                    mundaneOrMagicVocationExtras =
                        MundaneOrMagicVocationExtras.update
                            (MundaneOrMagicVocationExtras.Msg.SetLevelForVocationalSkills(
                                vocation.vocationStat.level,
                                dicePoolCalculationData
                            ))
                            vocation.mundaneOrMagicVocationExtras
            })

        | _ -> temp msg

    | MundaneOrMagicVocationExtrasMsg msg ->
        match msg with
        | MundaneOrMagicVocationExtras.Msg.MagicVocationExtrasMsg(MagicVocationExtras.Msg.MagicVocationSkillsMsg(MagicVocationSkills.Msg.ModifySkillAtPosition(pos,
                                                                                                                                                               msg))) ->
            match msg with
            | MagicVocationSkill.MagicSkillMsg(Skill.Msg.ModifySkillLevel(msg, _, dicePoolCalculationOption)) ->
                MagicVocationSkill.MagicSkillMsg(
                    Skill.Msg.ModifySkillLevel(msg, Some model.vocationStat.level, dicePoolCalculationOption)
                )
            | MagicVocationSkill.MundaneVocationSkillMsg(MundaneVocationSkill.SkillMsg(Skill.Msg.ModifySkillLevel(msg,
                                                                                                                  _,
                                                                                                                  dicePoolCalculationOption))) ->
                MagicVocationSkill.MundaneVocationSkillMsg(
                    MundaneVocationSkill.SkillMsg(
                        Skill.Msg.ModifySkillLevel(msg, Some model.vocationStat.level, dicePoolCalculationOption)
                    )
                )
            | _ -> msg
            |> (fun msg -> pos, msg)
            |> MagicVocationSkills.Msg.ModifySkillAtPosition
            |> MagicVocationExtras.Msg.MagicVocationSkillsMsg
            |> MundaneOrMagicVocationExtras.Msg.MagicVocationExtrasMsg

        | MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg(MundaneVocationSkills.ModifyMundaneVocationSkillAtPosition(pos,
                                                                                                                           msg)) ->
            match msg with
            | MundaneVocationSkill.SkillMsg(Skill.ModifySkillLevel(msg, _, dicePoolCalculationOption)) ->
                MundaneVocationSkill.SkillMsg(
                    Skill.ModifySkillLevel(msg, Some model.vocationStat.level, dicePoolCalculationOption)
                )
            | _ -> msg
            |> (fun msg -> pos, msg)
            |> MundaneVocationSkills.ModifyMundaneVocationSkillAtPosition
            |> MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg

        | _ -> msg

        |> (fun msg -> {
            model with
                mundaneOrMagicVocationExtras =
                    MundaneOrMagicVocationExtras.update msg model.mundaneOrMagicVocationExtras
        })

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