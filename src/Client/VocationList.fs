module VocationList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.MagicVocation
open FogentRoleplayLib.Vocation
open FogentRoleplayLib.Character

type Msg =
    | Modify of int * Vocation.Msg
    | InsertMundaneVocation of string
    | InsertMagicVocation of MagicSystem
    | Remove of int
    | CalculateDicePools of DicePoolCalculationData

let init () = []

let update msg model =
    match msg with
    | Modify(position, msg) ->
        model
        |> List.mapi (fun index vocation ->
            if index = position then
                Vocation.update msg vocation
            else
                vocation)
    | InsertMagicVocation magicSystem ->

        let currentMagicResource = 0u
        let initMundaneVocation = MundaneVocation.init ()

        {
            mundaneVocation = {
                initMundaneVocation with
                    vocationStat.name = magicSystem.vocationName
            }
            magicVocationSkillList = MagicSkillList.init ()
            magicSystem = magicSystem
            magicResourceCap = currentMagicResource
            currentMagicResource = currentMagicResource
        }
        |> MagicVocation
        |> List.singleton
        |> List.append model
    | Remove position -> List.removeAt position model
    | CalculateDicePools msg ->
        List.map (fun vocation -> Vocation.update (Vocation.CalculateDicePools(msg)) vocation) model


open Feliz
open Feliz.Bulma

let view attributeNameSet (vocationSkillData: VocationSkillData) model dispatch =
    Bulma.container [
        Bulma.label [ prop.text "Vocations and Vocational Skills:" ] |> Bulma.content
        //Bulma.button.button [ prop.onClick (fun _ -> dispatch Insert); prop.text "+" ]
        ViewUtils.textInputWithDropdownSet
            (fun input -> vocationSkillData.magicSystemMap.Item input |> InsertMagicVocation |> dispatch)
            (vocationSkillData.magicSystemMap.Keys)
            "vocationList"
        Bulma.columns [
            columns.isCentered
            prop.children [
                List.mapi
                    (fun position vocation ->
                        Bulma.column [
                            Vocation.view attributeNameSet vocationSkillData vocation (fun msg ->
                                dispatch (Modify(position, msg)))

                            Bulma.button.button [ prop.onClick (fun _ -> dispatch (Remove position)); prop.text "-" ]
                        ])
                    model
                |> Bulma.columns
            ]
        ]
    ]