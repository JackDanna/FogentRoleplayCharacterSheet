module MundaneVocationSkills

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.WeaponSkillData

type Msg =
    | ModifyMundaneVocationSkillAtPosition of int * MundaneVocationSkill.Msg
    | RemoveAtPosition of int
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceededForAll of Skill.CheckIfLevelCapExceeded
    | InsertMundaneVocationSkill of string * option<DicePoolCalculationData> * option<Map<string, WeaponSkillData>>

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
    | CheckIfLevelCapExceededForAll msgData ->
        model
        |> Set.map (fun skill ->
            MundaneVocationSkill.update (MundaneVocationSkill.CheckIfLevelCapExceeded msgData) skill)

    | InsertMundaneVocationSkill(skillName, Some dicePoolCalculationData, Some weaponSkillDataMap) ->
        MundaneVocationSkill.init weaponSkillDataMap dicePoolCalculationData skillName
        |> (fun x -> Set.add x model)

    | _ -> model

open Feliz
open Feliz.Bulma

let view attributeNameSet weaponSkillNames model (dispatch: Msg -> unit) =
    model
    |> Set.toList
    |> List.mapi (fun index mundaneVocationSkill ->
        (MundaneVocationSkill.view attributeNameSet mundaneVocationSkill (fun msg ->
            ModifyMundaneVocationSkillAtPosition(index, msg) |> dispatch))
        @ ViewUtils.deleteEquipmentRowButton (fun _ -> dispatch (RemoveAtPosition(index)))
        |> Bulma.columns
        |> Bulma.content)
    |> (fun x ->

        List.append x [
            ViewUtils.textInputWithDropdownSet
                (fun input -> InsertMundaneVocationSkill(input, None, None) |> dispatch)
                weaponSkillNames
                "mundaneVocationSkills"
        ])