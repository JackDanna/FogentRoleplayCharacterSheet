module MagicVocationSkills

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.WeaponSkillData
open FogentRoleplayLib.MagicSkillData
open FogentRoleplayLib.MagicVocationSkill
open FogentRoleplayLib.AttributeName

type Msg =
    | ModifySkillAtPosition of int * MagicVocationSkill.Msg
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceededForAll of Skill.CheckIfLevelCapExceeded
    | InsertSkill of
        string *
        option<AttributeName Set> *
        option<DicePoolCalculationData> *
        option<Map<string, WeaponSkillData>> *
        option<Map<string, MagicSkillData>>

let init () = Set.empty

let update msg (model: MagicVocationSkill Set) =
    match msg with
    | ModifySkillAtPosition(position, msg) ->
        model
        |> Set.toList
        |> List.mapi (fun index coreSkill ->
            if index = position then
                MagicVocationSkill.update msg coreSkill
            else
                coreSkill)
        |> Set.ofList
    | CalculateDicePools dicePoolCalculationData ->
        Set.map
            (fun coreSkill ->
                MagicVocationSkill.update (MagicVocationSkill.CalculateDicePool(dicePoolCalculationData)) coreSkill)
            model
    | CheckIfLevelCapExceededForAll msgData ->
        model
        |> Set.map (fun skill -> MagicVocationSkill.update (MagicVocationSkill.CheckIfLevelCapExceeded msgData) skill)
    | InsertSkill(skillName,
                  Some vocationGoverningAttributeNames,
                  Some dicePoolCalculationData,
                  Some weaponSkillDataMap,
                  Some magicSkillDataMap) ->

        match magicSkillDataMap.TryFind skillName with
        | Some magicSkillData ->

            MagicVocationSkill.initMagicSkill
                magicSkillData.name
                vocationGoverningAttributeNames
                dicePoolCalculationData

        | None ->
            MundaneVocationSkill.init weaponSkillDataMap dicePoolCalculationData skillName
            |> MundaneVocationSkill
        |> (fun x -> Set.add x model)

    | _ -> model

open Feliz
open Feliz.Bulma

let view attributeNameSet weaponSkillNames model (dispatch: Msg -> unit) =
    model
    |> Set.toList
    |> List.mapi (fun index mundaneVocationSkill ->
        (MagicVocationSkill.view attributeNameSet mundaneVocationSkill (fun msg ->
            ModifySkillAtPosition(index, msg) |> dispatch))
        |> Bulma.content)
    |> (fun x ->

        List.append x [
            ViewUtils.textInputWithDropdownSet
                (fun input -> InsertSkill(input, None, None, None, None) |> dispatch)
                weaponSkillNames
                "mundaneVocationSkills"
        ]

    )