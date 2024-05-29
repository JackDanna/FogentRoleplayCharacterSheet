module VocationList

open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.Skill

type Msg =
    | VocationMsgAtPosition of int * Vocation.Msg
    | InsertVocation of
        string *
        option<Map<string, Skill>> *
        option<DicePoolCalculationData> *
        option<Map<string, MagicSystem>>
    | Remove of int
    | CalculateDicePools of DicePoolCalculationData

let init () = []

let update msg model =
    match msg with
    | VocationMsgAtPosition(position, msg) ->
        model
        |> List.mapi (fun index vocation ->
            if index = position then
                Vocation.update msg vocation
            else
                vocation)
    | InsertVocation(vocationName, Some coreSkillMap, Some dicePoolCalculationData, Some magicSystemMap) ->
        Vocation.init vocationName coreSkillMap dicePoolCalculationData magicSystemMap
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
        ViewUtils.textInputWithDropdownSet
            (fun input -> InsertVocation(input, None, None, None) |> dispatch)
            (magicSystemNameSet)
            "vocationList"
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
    ]