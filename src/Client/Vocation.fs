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
    | VocationStatMsg(msg) ->
        match msg with
        | VocationStat.ToggleGoveringAttribute(attributeName, dicePoolCalculationDataOption) ->
            (VocationStat.ToggleGoveringAttribute(attributeName, dicePoolCalculationDataOption))
            |> temp

        | VocationStat.ZeroToFiveMsg(msg, Some dicePoolCalculationData) ->
            (VocationStat.ZeroToFiveMsg(msg, Some dicePoolCalculationData))
            |> temp
            |> (fun vocation -> {
                vocation with
                    mundaneOrMagicVocationExtras =
                        vocation.mundaneOrMagicVocationExtras
                        |> MundaneOrMagicVocationExtras.update (
                            MundaneOrMagicVocationExtras.Msg.SetLevelForVocationalSkills(
                                vocation.vocationStat.level,
                                dicePoolCalculationData
                            )
                        )
                        |> MundaneOrMagicVocationExtras.update (
                            MundaneOrMagicVocationExtras.Msg.CheckIfLevelCapExceededForSkills(
                                vocation.vocationStat.level,
                                dicePoolCalculationData
                            )
                        )
            })

        | _ -> temp msg

    | MundaneOrMagicVocationExtrasMsg msg ->
        match msg with
        | MundaneOrMagicVocationExtras.MagicVocationExtrasMsg(MagicVocationExtras.MagicVocationSkillsMsg(msg)) ->
            match msg with
            | MagicVocationSkills.ModifySkillAtPosition(pos, msg) ->
                match msg with
                | MagicVocationSkill.MagicSkillMsg(Skill.ModifySkillLevel(msg, _, dicePoolCalculationOption)) ->
                    MagicVocationSkill.MagicSkillMsg(
                        Skill.ModifySkillLevel(msg, Some model.vocationStat.level, dicePoolCalculationOption)
                    )
                | MagicVocationSkill.MundaneVocationSkillMsg(MundaneVocationSkill.SkillMsg(Skill.ModifySkillLevel(msg,
                                                                                                                  _,
                                                                                                                  dicePoolCalculationOption))) ->
                    MagicVocationSkill.MundaneVocationSkillMsg(
                        MundaneVocationSkill.SkillMsg(
                            Skill.ModifySkillLevel(msg, Some model.vocationStat.level, dicePoolCalculationOption)
                        )
                    )
                | _ -> msg
                |> (fun msg -> pos, msg)
                |> MagicVocationSkills.ModifySkillAtPosition

            | MagicVocationSkills.InsertMagicVocationSkill(a, _, b, c, d, e) ->
                MagicVocationSkills.InsertMagicVocationSkill(a, Some model.vocationStat.level, b, c, d, e)

            | _ -> msg

            |> MagicVocationExtras.MagicVocationSkillsMsg
            |> MundaneOrMagicVocationExtras.MagicVocationExtrasMsg

        | MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg(msg) ->
            match msg with
            | MundaneVocationSkills.ModifyMundaneVocationSkillAtPosition(pos,
                                                                         MundaneVocationSkill.SkillMsg(Skill.ModifySkillLevel(msg,
                                                                                                                              _,
                                                                                                                              dicePoolCalculationOption))) ->

                MundaneVocationSkills.ModifyMundaneVocationSkillAtPosition(
                    pos,
                    MundaneVocationSkill.SkillMsg(
                        Skill.ModifySkillLevel(msg, Some model.vocationStat.level, dicePoolCalculationOption)
                    )
                )

            | MundaneVocationSkills.InsertMundaneVocationSkill(a, _, b, c) ->
                MundaneVocationSkills.InsertMundaneVocationSkill(a, Some model.vocationStat.level, b, c)

            | _ -> msg

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