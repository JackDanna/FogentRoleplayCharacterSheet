module AttributeStat

open FogentRoleplayLib.AttributeStat

type Model = AttributeStat

type Msg =
    Neg2To5Msg of Neg2To5.Msg

let init () = {
    attribute = "STR";
    stat = Neg2To5.init();
}

let update msg model =
    match msg with
    | Neg2To5Msg msg ->
        { model with stat = Neg2To5.update msg model.stat}

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.columns [
        Bulma.column [ prop.text model.attribute ]
        Bulma.column [
            Neg2To5.view model.stat (Neg2To5Msg >> dispatch)
        ]
    ]