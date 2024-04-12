module VocationList

open FogentRoleplayLib.Vocation
open FogentRoleplayLib.AttributeName

type Msg =
    | Modify of int * Vocation.Msg
    | Insert
    | Remove of int

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
                    (fun position vocationTable ->
                        Bulma.column [
                            Vocation.view attributeNameSet vocationTable (fun msg -> dispatch (Modify(position, msg)))

                            Bulma.button.button [ prop.onClick (fun _ -> dispatch (Remove position)); prop.text "-" ]
                        ])
                    model
                |> Bulma.columns
            ]
        ]
    ]