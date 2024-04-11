module VocationSkill

open FogentRoleplayLib.VocationSkill

type Msg =
    | VocationalSkillMsg of VocationalSkill.Msg
    | WeaponSkillMsg of VocationalSkill.Msg
    | MagicSkillMsg of VocationalSkill.Msg

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
    | _ -> model

open Feliz
open Feliz.Bulma

let view model dispatch =
    match model with
    | VocationalSkill vocationalSkill -> VocationalSkill.view vocationalSkill (VocationalSkillMsg >> dispatch) false
    | WeaponSkill vocationalSkill -> VocationalSkill.view vocationalSkill (VocationalSkillMsg >> dispatch) true
    | MagicSkill magicSkill -> VocationalSkill.view magicSkill.vocationalSkill (VocationalSkillMsg >> dispatch) true