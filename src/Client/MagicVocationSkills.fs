module MagicVocationSkills

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.WeaponSkillData
open FogentRoleplayLib.MagicSkillData
open FogentRoleplayLib.MagicVocationSkill
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.ZeroToFive

type Msg =
    | ModifySkillAtPosition of int * MagicVocationSkill.Msg
    | RemoveAtPosition of int
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceededForSkills of Skill.ZeroToFiveAndDicePoolCalculationData
    | InsertMagicVocationSkill of
        string *
        option<ZeroToFive> *
        option<AttributeName Set> *
        option<DicePoolCalculationData> *
        option<Map<string, WeaponSkillData>> *
        option<MagicSkillData Set>
    | SetLevelForVocationalSkills of Skill.ZeroToFiveAndDicePoolCalculationData

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
    | RemoveAtPosition position -> model |> Set.toList |> List.removeAt position |> Set.ofList
    | CalculateDicePools dicePoolCalculationData ->
        Set.map
            (fun coreSkill ->
                MagicVocationSkill.update (MagicVocationSkill.CalculateDicePool(dicePoolCalculationData)) coreSkill)
            model
    | CheckIfLevelCapExceededForSkills msgData ->
        model
        |> Set.map (fun skill -> MagicVocationSkill.update (MagicVocationSkill.CheckIfLevelCapExceeded msgData) skill)
    | InsertMagicVocationSkill(skillName,
                               Some vocationStatLevel,
                               Some vocationGoverningAttributeNames,
                               Some dicePoolCalculationData,
                               Some weaponSkillDataMap,
                               Some magicSkillDataSet) ->

        match (makeMagicSkillDataMap magicSkillDataSet).TryFind skillName with
        | Some magicSkillData ->

            MagicVocationSkill.initMagicSkill
                magicSkillData.name
                vocationGoverningAttributeNames
                dicePoolCalculationData

        | None ->
            MundaneVocationSkill.init weaponSkillDataMap dicePoolCalculationData skillName vocationStatLevel
            |> MundaneVocationSkill
        |> (fun x -> Set.add x model)
    | SetLevelForVocationalSkills data ->
        model
        |> Set.map (fun magicVocationSkill ->
            MagicVocationSkill.update
                (MagicVocationSkill.Msg.MundaneVocationSkillMsg(
                    MundaneVocationSkill.Msg.SetLevelForVocationalSkill(data)
                ))
                magicVocationSkill)

    | _ -> model

open Feliz

let view magicSystemName attributeNameSet magicSkillNames weaponSkillNames model (dispatch: Msg -> unit) =
    model
    |> Seq.mapi (fun index mundaneVocationSkill ->
        Seq.append
            (MagicVocationSkill.view
                //
                attributeNameSet
                mundaneVocationSkill
                (fun msg -> ModifySkillAtPosition(index, msg) |> dispatch))
            (ViewUtils.deleteEquipmentRowButton (fun _ -> dispatch (RemoveAtPosition(index)))))
    |> (fun magicVocationSkillsTableBody ->

        magicVocationSkillsTableBody,
        ViewUtils.textInputWithDropdownSet
            (fun input -> InsertMagicVocationSkill(input, None, None, None, None, None) |> dispatch)
            (Set.union magicSkillNames weaponSkillNames)
            magicSystemName)