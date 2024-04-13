module VocationList

open FogentRoleplayLib.DicePoolCalculation

type Msg =
    | Modify of int * Vocation.Msg
    | Insert
    | Remove of int
    | CalculateDicePools of DicePoolCalculationData

let init () = [ Vocation.init () ]

let update msg model =
    match msg with
    | Modify(position, msg) ->
        model
        |> List.mapi (fun index vocation ->
            if index = position then
                Vocation.update msg vocation
            else
                vocation)
    | Insert -> List.append model [ Vocation.init () ]
    | Remove position -> List.removeAt position model
    | CalculateDicePools msg ->
        List.map (fun vocation -> Vocation.update (Vocation.CalculateDicePools(msg)) vocation) model


open Feliz
open Feliz.Bulma

let view attributeNameSet model dispatch =
    Bulma.container [
        Bulma.label [ prop.text "Vocations and Vocational Skills:" ] |> Bulma.content
        Bulma.button.button [ prop.onClick (fun _ -> dispatch Insert); prop.text "+" ]
        Bulma.columns [
            columns.isCentered
            prop.children [
                List.mapi
                    (fun position vocation ->
                        Bulma.column [
                            Vocation.view attributeNameSet vocation (fun msg -> dispatch (Modify(position, msg)))

                            Bulma.button.button [ prop.onClick (fun _ -> dispatch (Remove position)); prop.text "-" ]
                        ])
                    model
                |> Bulma.columns
            ]
        ]
    ]