module VocationSkill

open FogentRoleplayLib.VocationSkill
open FogentRoleplayLib.Neg1To5
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.ZeroToFive

type Msg =
    | VocationalSkillMsg of VocationalSkill.Msg
    | WeaponSkillMsg of VocationalSkill.Msg
    | MagicSkillMsg of VocationalSkill.Msg
    | CalculateDicePools of DicePoolCalculationData
    | SetSkillLevel of Neg1To5
    | CheckIfLevelCapExceeded of ZeroToFive

let init () =
    VocationalSkill.init () |> VocationalSkill

let update msg (model: VocationSkill) : VocationSkill =
    match msg, model with
    | VocationalSkillMsg msg, VocationalSkill vocationalSkill ->
        VocationalSkill.update msg vocationalSkill |> VocationalSkill
    | WeaponSkillMsg msg, WeaponSkill vocationalSkill -> VocationalSkill.update msg vocationalSkill |> WeaponSkill
    | MagicSkillMsg msg, MagicSkill magicSkill ->
        {
            magicSkill with
                vocationalSkill = VocationalSkill.update msg magicSkill.vocationalSkill
        }
        |> MagicSkill
    | CalculateDicePools dicePoolCalculationData, _ ->
        match model with
        | VocationalSkill vocationalSkill ->
            VocationalSkill.update (VocationalSkill.CalculateDicePool(dicePoolCalculationData)) vocationalSkill
            |> VocationalSkill
        | WeaponSkill vocationalSkill ->
            VocationalSkill.update (VocationalSkill.CalculateDicePool(dicePoolCalculationData)) vocationalSkill
            |> WeaponSkill
        | MagicSkill magicSkill ->
            {
                magicSkill with
                    vocationalSkill =
                        VocationalSkill.update
                            (VocationalSkill.CalculateDicePool(dicePoolCalculationData))
                            magicSkill.vocationalSkill
            }
            |> MagicSkill
    | SetSkillLevel newSkillLevel, _ ->
        match model with
        | VocationalSkill vocationalSkill ->
            VocationalSkill.update (VocationalSkill.SetSkillLevel(newSkillLevel)) vocationalSkill
            |> VocationalSkill
        | WeaponSkill vocationalSkill ->
            VocationalSkill.update (VocationalSkill.SetSkillLevel(newSkillLevel)) vocationalSkill
            |> WeaponSkill
        | MagicSkill magicSkill ->
            {
                magicSkill with
                    vocationalSkill =
                        VocationalSkill.update (VocationalSkill.SetSkillLevel(newSkillLevel)) magicSkill.vocationalSkill
            }
            |> MagicSkill
    | CheckIfLevelCapExceeded levelCap, _ ->
        match model with
        | VocationalSkill vocationalSkill ->
            VocationalSkill.update (VocationalSkill.CheckIfLevelCapExceeded(levelCap)) vocationalSkill
            |> VocationalSkill
        | WeaponSkill vocationalSkill ->
            VocationalSkill.update (VocationalSkill.CheckIfLevelCapExceeded(levelCap)) vocationalSkill
            |> WeaponSkill
        | MagicSkill magicSkill ->
            {
                magicSkill with
                    vocationalSkill =
                        VocationalSkill.update
                            (VocationalSkill.CheckIfLevelCapExceeded(levelCap))
                            magicSkill.vocationalSkill
            }
            |> MagicSkill

    | _ -> model

open Feliz
open Feliz.Bulma

let view attributeNameSet model dispatch =
    match model with
    | VocationalSkill vocationalSkill ->
        VocationalSkill.view attributeNameSet vocationalSkill (VocationalSkillMsg >> dispatch) false
    | WeaponSkill vocationalSkill ->
        VocationalSkill.view attributeNameSet vocationalSkill (VocationalSkillMsg >> dispatch) true
    | MagicSkill magicSkill ->
        VocationalSkill.view attributeNameSet magicSkill.vocationalSkill (VocationalSkillMsg >> dispatch) true