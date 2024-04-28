module MundanceVocationSkill

open FogentRoleplayLib.MundaneVocationSkill
open FogentRoleplayLib.Neg1To5
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.ZeroToFive

type Msg =
    | VocationalSkillMsg of VocationalSkill.Msg
    | WeaponSkillMsg of VocationalSkill.Msg
    | CalculateDicePools of DicePoolCalculationData
    | SetSkillLevel of Neg1To5
    | CheckIfLevelCapExceeded of ZeroToFive

let init () =
    VocationalSkill.init () |> VocationalSkill

let update msg (model: MundaneVocationSkill) : MundaneVocationSkill =
    match msg, model with
    | VocationalSkillMsg msg, VocationalSkill vocationalSkill ->
        VocationalSkill.update msg vocationalSkill |> VocationalSkill

    | WeaponSkillMsg msg, WeaponSkill vocationalSkill -> VocationalSkill.update msg vocationalSkill |> WeaponSkill

    | CalculateDicePools dicePoolCalculationData, _ ->
        let newVocationalSkill =
            VocationalSkill.update (VocationalSkill.CalculateDicePool(dicePoolCalculationData)) vocationalSkill

        match model with
        | VocationalSkill vocationalSkill -> newVocationalSkill |> VocationalSkill
        | WeaponSkill vocationalSkill -> newVocationalSkill |> WeaponSkill

    | SetSkillLevel newSkillLevel, _ ->
        match model with
        | VocationalSkill vocationalSkill ->
            VocationalSkill.update (VocationalSkill.SetSkillLevel(newSkillLevel)) vocationalSkill
            |> VocationalSkill
        | WeaponSkill vocationalSkill ->
            VocationalSkill.update (VocationalSkill.SetSkillLevel(newSkillLevel)) vocationalSkill
            |> WeaponSkill

    | CheckIfLevelCapExceeded levelCap, _ ->
        match model with
        | VocationalSkill vocationalSkill ->
            VocationalSkill.update (VocationalSkill.CheckIfLevelCapExceeded(levelCap)) vocationalSkill
            |> VocationalSkill
        | WeaponSkill vocationalSkill ->
            VocationalSkill.update (VocationalSkill.CheckIfLevelCapExceeded(levelCap)) vocationalSkill
            |> WeaponSkill

    | _ -> model

open Feliz
open Feliz.Bulma

let view attributeNameSet model dispatch =
    match model with
    | VocationalSkill vocationalSkill ->
        VocationalSkill.view attributeNameSet vocationalSkill (VocationalSkillMsg >> dispatch) true
    | WeaponSkill vocationalSkill ->
        VocationalSkill.view attributeNameSet vocationalSkill (WeaponSkillMsg >> dispatch) false