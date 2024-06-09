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
    | VocationStatMsg(msg) ->
        match msg with
        | VocationStat.ToggleGoveringAttribute(attributeName, dicePoolCalculationDataOption) ->
            (VocationStat.ToggleGoveringAttribute(attributeName, dicePoolCalculationDataOption))

        | VocationStat.Msg.ZeroToFiveMsg(msg, dicePoolCalculationData) ->
            (VocationStat.Msg.ZeroToFiveMsg(msg, dicePoolCalculationData))

        | _ -> msg
        |> (fun msg ->
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

    | MundaneOrMagicVocationExtrasMsg msg ->
        match msg with
        | MundaneOrMagicVocationExtras.Msg.MagicVocationExtrasMsg(MagicVocationExtras.Msg.MagicVocationSkillsMsg(MagicVocationSkills.Msg.ModifySkillAtPosition(pos,
                                                                                                                                                               msg))) ->
            match msg with
            | MagicVocationSkill.SkillMsg(Skill.Msg.ModifySkillLevel(msg, _, dicePoolCalculationOption)) ->
                MagicVocationSkill.SkillMsg(
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