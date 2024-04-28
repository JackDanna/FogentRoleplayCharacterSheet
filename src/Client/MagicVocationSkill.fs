module MagicVocationSkill

open FogentRoleplayLib.MagicVocation

open FogentRoleplayLib.Neg1To5
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.ZeroToFive

type Msg =
    | MundaneVocationSkillMsg of MundaneVocationSkill.Msg
    | MagicSkillMsg of VocationalSkill.Msg
    | CalculateDicePools of DicePoolCalculationData
    | SetSkillLevel of Neg1To5
    | CheckIfLevelCapExceeded of ZeroToFive

// let init () =
//     VocationalSkill.init () |> MundaneVocationSkill

let update msg (model: MagicVocationSkill) : MagicVocationSkill =
    match msg, model with
    | MundaneVocationSkillMsg msg, MundaneVocationSkill mundaneVocationSkill ->
        MundaneVocationSkill.update msg mundaneVocationSkill |> MundaneVocationSkill
    | MagicSkillMsg msg, MagicSkill magicSkill ->
        {
            magicSkill with
                vocationalSkill = VocationalSkill.update msg magicSkill.vocationalSkill
        }
        |> MagicSkill
    | CalculateDicePools dicePoolCalculationData, _ ->
        match model with
        | MundaneVocationSkill mundaneVocationSkill ->
            MundaneVocationSkill.update
                (MundaneVocationSkill.CalculateDicePools dicePoolCalculationData)
                mundaneVocationSkill
            |> MundaneVocationSkill
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
        | MundaneVocationSkill mundaneVocationSkill ->
            MundaneVocationSkill.update (MundaneVocationSkill.SetSkillLevel newSkillLevel) mundaneVocationSkill
            |> MundaneVocationSkill
        | MagicSkill magicSkill ->
            {
                magicSkill with
                    vocationalSkill =
                        VocationalSkill.update (VocationalSkill.SetSkillLevel(newSkillLevel)) magicSkill.vocationalSkill
            }
            |> MagicSkill
    | CheckIfLevelCapExceeded levelCap, _ ->
        match model with
        | MundaneVocationSkill mundanceVocationSkill ->
            MundaneVocationSkill.update (MundaneVocationSkill.CheckIfLevelCapExceeded levelCap) mundanceVocationSkill
            |> MundaneVocationSkill
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
    | MundaneVocationSkill mundaneVocationSkill ->
        MundaneVocationSkill.view attributeNameSet mundaneVocationSkill (MundaneVocationSkillMsg >> dispatch)
    | MagicSkill magicSkill ->
        VocationalSkill.view attributeNameSet magicSkill.vocationalSkill (MagicSkillMsg >> dispatch) false