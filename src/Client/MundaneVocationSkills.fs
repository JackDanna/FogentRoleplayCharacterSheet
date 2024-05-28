module MundaneVocationSkills

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.WeaponSkillData

type Msg =
    | ModifySkillAtPosition of int * MundaneVocationSkill.Msg
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceededForAll of Skill.CheckIfLevelCapExceeded
    | InsertSkill of string * option<Map<string, WeaponSkillData>> * option<DicePoolCalculationData>

let init () = Set.empty

let insertMundaneVocationSkill (weaponSkillDataMap: Map<string, WeaponSkillData>) dicePoolCalculationData skillName =
    match weaponSkillDataMap.TryFind skillName with
    | Some weaponSkillData ->
        MundaneVocationSkill.initWeaponSkill
            weaponSkillData.name
            weaponSkillData.governingAttributes
            dicePoolCalculationData
    | None -> MundaneVocationSkill.initVocationalSkill skillName Set.empty dicePoolCalculationData


let update msg model =
    match msg with
    | ModifySkillAtPosition(position, msg) ->
        model
        |> Set.toList
        |> List.mapi (fun index coreSkill ->
            if index = position then
                MundaneVocationSkill.update msg coreSkill
            else
                coreSkill)
        |> Set.ofList
    | CalculateDicePools dicePoolCalculationData ->
        Set.map
            (fun coreSkill ->
                MundaneVocationSkill.update (MundaneVocationSkill.CalculateDicePool(dicePoolCalculationData)) coreSkill)
            model
    | CheckIfLevelCapExceededForAll msgData ->
        model
        |> Set.map (fun skill ->
            MundaneVocationSkill.update (MundaneVocationSkill.CheckIfLevelCapExceeded msgData) skill)
    | InsertSkill(skillName, Some weaponSkillDataMap, Some dicePoolCalculationData) ->

        insertMundaneVocationSkill weaponSkillDataMap dicePoolCalculationData skillName
        |> (fun x -> Set.add x model)
    | _ -> model

open Feliz
open Feliz.Bulma

let view attributeNameSet weaponSkillNames model (dispatch: Msg -> unit) =
    model
    |> Set.toList
    |> List.mapi (fun index mundaneVocationSkill ->
        (MundaneVocationSkill.view attributeNameSet mundaneVocationSkill (fun msg ->
            ModifySkillAtPosition(index, msg) |> dispatch))
        |> Bulma.content)
    |> (fun x ->

        List.append x [
            ViewUtils.textInputWithDropdownSet
                (fun input -> InsertSkill(input, None, None) |> dispatch)
                weaponSkillNames
                "mundaneVocationSkills"
        ]

    )
//|> Html.ul