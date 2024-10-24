module MundaneVocationSkills

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.WeaponSkillData
open FogentRoleplayLib.ZeroToFive

type Msg =
    | ModifyMundaneVocationSkillAtPosition of int * MundaneVocationSkill.Msg
    | RemoveAtPosition of int
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceededForSkills of Skill.ZeroToFiveAndDicePoolCalculationData
    | InsertMundaneVocationSkill of
        string *
        option<ZeroToFive> *
        option<DicePoolCalculationData> *
        option<Map<string, WeaponSkillData>>
    | SetLevelForVocationalSkills of Skill.ZeroToFiveAndDicePoolCalculationData

let init () = Set.empty

let update msg model =
    match msg with
    | ModifyMundaneVocationSkillAtPosition(position, msg) ->
        model
        |> Set.toList
        |> List.mapi (fun index mundaneVocationSkill ->
            if index = position then
                MundaneVocationSkill.update msg mundaneVocationSkill
            else
                mundaneVocationSkill)
        |> Set.ofList
    | RemoveAtPosition position -> model |> List.ofSeq |> List.removeAt position |> Set.ofList
    | CalculateDicePools dicePoolCalculationData ->
        Set.map
            (fun coreSkill ->
                MundaneVocationSkill.update (MundaneVocationSkill.CalculateDicePool(dicePoolCalculationData)) coreSkill)
            model
    | CheckIfLevelCapExceededForSkills msgData ->
        model
        |> Set.map (fun skill ->
            MundaneVocationSkill.update (MundaneVocationSkill.CheckIfLevelCapExceeded msgData) skill)

    | InsertMundaneVocationSkill(skillName,
                                 Some vocationStatLevel,
                                 Some dicePoolCalculationData,
                                 Some weaponSkillDataMap) ->
        MundaneVocationSkill.init weaponSkillDataMap dicePoolCalculationData skillName vocationStatLevel
        |> (fun x -> Set.add x model)

    | SetLevelForVocationalSkills data ->
        model
        |> Set.map (fun mundaneVocationSkill ->
            MundaneVocationSkill.update (MundaneVocationSkill.Msg.SetLevelForVocationalSkill data) mundaneVocationSkill)

    | _ -> model

open Feliz

let view attributeNameSet weaponSkillNames model (dispatch: Msg -> unit) =
    model
    |> Seq.mapi (fun index mundaneVocationSkill ->
        Seq.append
            (MundaneVocationSkill.view
                //
                attributeNameSet
                mundaneVocationSkill
                (fun msg -> ModifyMundaneVocationSkillAtPosition(index, msg) |> dispatch))
            (ViewUtils.deleteEquipmentRowButton (fun _ -> dispatch (RemoveAtPosition(index)))))
    |> (fun mundaneVocationSkillsTableBody ->
        mundaneVocationSkillsTableBody,
        ViewUtils.textInputWithDropdownSet
            (fun input -> InsertMundaneVocationSkill(input, None, None, None) |> dispatch)
            weaponSkillNames
            "mundaneVocationSkills")