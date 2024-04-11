module VocationSkill

open FogentRoleplayLib.VocationSkill

type Msg =
    | VocationalSkillMsg of Skill.Msg
    | WeaponSkillMsg of Skill.Msg
    | MagicSkillMsg of Skill.Msg

let update msg (model: VocationSkill) : VocationSkill =
    match msg, model with
    | VocationalSkillMsg msg, VocationalSkill vocationalSkill ->
        {
            vocationalSkill with
                skill = Skill.update msg vocationalSkill.skill
        }
        |> VocationalSkill
    | WeaponSkillMsg msg, WeaponSkill vocationalSkill ->
        {
            vocationalSkill with
                skill = Skill.update msg vocationalSkill.skill
        }
        |> WeaponSkill
    | MagicSkillMsg msg, MagicSkill magicSkill ->
        {
            magicSkill with
                vocationalSkill.skill = Skill.update msg magicSkill.vocationalSkill.skill
        }
        |> MagicSkill
    | _ -> model

open Feliz
open Feliz.Bulma

let view model dispatch =
    match model with
    | VocationalSkill vocationalSkill -> VocationalSkill.view vocationalSkill (VocationalSkillMsg >> dispatch)