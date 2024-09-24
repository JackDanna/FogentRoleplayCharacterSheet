module VocationList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.Skill

type Msg =
    | VocationMsgAtPosition of int * Vocation.Msg
    | VocationMsgForAll of Vocation.Msg
    | InsertVocation of string * option<Map<string, Skill>> * option<DicePoolCalculationData> * option<MagicSystem Set>
    | Remove of int
    | CalculateDicePools of DicePoolCalculationData

let init () = FogentRoleplayLib.ListUtils.init

let update msg model =
    match msg with
    | VocationMsgAtPosition(position, msg) ->
        model
        |> List.mapi (fun index vocation ->
            if index = position then
                Vocation.update msg vocation
            else
                vocation)
    | VocationMsgForAll msg -> List.map (fun vocation -> Vocation.update msg vocation) model
    | InsertVocation(vocationName, Some coreSkillMap, Some dicePoolCalculationData, Some magicSystemSet) ->
        Vocation.init vocationName coreSkillMap dicePoolCalculationData (makeMagicSystemDataMap magicSystemSet)
        |> List.singleton
        |> List.append model

    | Remove position -> List.removeAt position model
    | CalculateDicePools msg ->
        List.map (fun vocation -> Vocation.update (Vocation.CalculateDicePools(msg)) vocation) model
    | _ -> model


open Feliz
open Feliz.Bulma

let view attributeNameSet (magicSystemNameSet) (weaponSkillNameSet) model dispatch =
    Bulma.container [
        Bulma.label [ prop.text "Vocations and Vocational Skills:" ] |> Bulma.content
        //Bulma.button.button [ prop.onClick (fun _ -> dispatch Insert); prop.text "+" ]
        Bulma.columns [
            columns.isCentered
            prop.children [
                List.mapi
                    (fun position vocation ->
                        Bulma.column [
                            Vocation.view attributeNameSet weaponSkillNameSet vocation (fun msg ->
                                dispatch (VocationMsgAtPosition(position, msg)))

                            Bulma.button.button [ prop.onClick (fun _ -> dispatch (Remove position)); prop.text "-" ]
                        ])
                    model
                |> Bulma.columns
            ]
        ]
        ViewUtils.textInputWithDropdownSet
            (fun input -> InsertVocation(input, None, None, None) |> dispatch)
            (magicSystemNameSet)
            "vocationList"
    ]