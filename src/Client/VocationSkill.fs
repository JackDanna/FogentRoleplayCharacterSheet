module VocationSkill

open FogentRoleplayLib.VocationSkill

type Msg =
    | VocationSkillMsg of VocationalSkill.Msg
    | WeaponSkillMsg of VocationalSkill.Msg
    | MagicSkillMsg of VocationalSkill.Msg