module MagicVocationSkillList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.MagicVocationSkill
open FogentRoleplayLib.MagicSkill
open FogentRoleplayLib.ZeroToFive

type Msg =
    | InsertMagicSkill of MagicSkill
    | RemoveAtPostion of int
    | ModifiedVocationSkillAtPosition of int * MundaneVocationSkill.Msg
    | CalculateDicePools of DicePoolCalculationData
    | CheckIfLevelCapExceeded of int * ZeroToFive
    | CheckIfLevelCapExeededForAll of ZeroToFive

let init () = []

let update msg model =
    match msg with
    | InsertVocationalSkill name ->
        let initVocationalSkill = VocationalSkill.init ()

        {
            initVocationalSkill with
                skill.name = name
        }
        |> VocationalSkill
        |> List.singleton
        |> List.append model
    | InsertWeaponSkill name ->
        let initVocationalSkill = VocationalSkill.init ()

        {
            initVocationalSkill with
                skill.name = name
        }
        |> WeaponSkill
        |> List.singleton
        |> List.append model
    // | InsertMagicSkill magicSkillData ->

    //     let initVocationalSkill = VocationalSkill.init ()

    //     {
    //         vocationalSkill = {
    //             initVocationalSkill with
    //                 skill.name = magicSkillData.name
    //         }
    //         magicSkillData = magicSkillData
    //     }
    //     |> MagicSkill
    //     |> List.singleton
    //     |> List.append model

    | RemoveAtPostion position -> List.removeAt position model
    | ModifiedVocationSkillAtPosition(position, msg) ->
        model
        |> List.mapi (fun index vocationSkill ->
            if index = position then
                MundaneVocationSkill.update msg vocationSkill
            else
                vocationSkill)
    | CalculateDicePools dicePoolCalculationData ->
        List.map
            (fun vocationSkill ->
                MundaneVocationSkill.update
                    (MundaneVocationSkill.CalculateDicePools(dicePoolCalculationData))
                    vocationSkill)
            model
    | CheckIfLevelCapExceeded(position, levelCap) ->
        List.mapi
            (fun index vocationSkill ->
                if index = position then
                    MundaneVocationSkill.update (MundaneVocationSkill.CheckIfLevelCapExceeded(levelCap)) vocationSkill
                else
                    vocationSkill)

            model
    | CheckIfLevelCapExeededForAll levelCap ->
        model
        |> List.map (fun vocationSkill ->
            MundaneVocationSkill.update (MundaneVocationSkill.CheckIfLevelCapExceeded levelCap) vocationSkill)

open Feliz
open Feliz.Bulma

let view attributeNameSet weaponGoverningSkillNameSet (model: MundaneVocationSkill list) dispatch =
    List.append
        (List.mapi
            (fun position skillRow ->
                MundaneVocationSkill.view attributeNameSet skillRow (fun (msg: MundaneVocationSkill.Msg) ->
                    ((ModifiedVocationSkillAtPosition(position, msg)) |> dispatch))
                @ [
                    Bulma.column [
                        Html.button [ prop.onClick (fun _ -> dispatch (RemoveAtPostion position)); prop.text "-" ]
                    ]
                ]
                |> Bulma.columns
                |> Bulma.content)
            model)
        [
            Bulma.input.text [
                prop.list "vocationSkillNameSet"
                prop.onTextChange (fun input ->
                    // if Seq.contains input (Seq.map (_.name) magicSkillDataSet) then
                    //     (Seq.find (fun magicSkillData -> magicSkillData.name = input) magicSkillDataSet)
                    //     |> InsertMagicSkill
                    // el
                    if Seq.contains input weaponGoverningSkillNameSet then
                        InsertWeaponSkill input
                    else
                        InsertVocationalSkill input
                    |> dispatch)
            ]
            Html.datalist [
                prop.id "vocationSkillNameSet"
                prop.children (
                    //[ (Seq.map (_.name) magicSkillDataSet); weaponGoverningSkillNameSet ]

                    //|> Seq.collect id
                    weaponGoverningSkillNameSet
                    |> Seq.map (fun (itemName: string) -> Html.option [ prop.value itemName ])
                )
            ]
        ]
    |> Html.ul