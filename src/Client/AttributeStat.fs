module AttributeStat

open FogentRoleplayLib.Attribute

type Model = Attribute

type Msg = Neg2To5Msg of Neg2To5.Msg

let init () = {
    attributeName = ""
    level = Neg2To5.init ()
}

let update msg model =
    match msg with
    | Neg2To5Msg msg -> {
        model with
            level = Neg2To5.update msg model.level
      }

open Feliz
open Feliz.Bulma

let view model dispatch =
    Bulma.columns [
        Bulma.column [ prop.text model.attributeName ]
        Bulma.column [ Neg2To5.view model.level (Neg2To5Msg >> dispatch) ]
    ]