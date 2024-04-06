module Neg2To5

open FogentRoleplayLib.Neg2To5

type Model = Neg2To5

type Msg =
    | ChangedNeg2To5 of int

let init () = Zero

let update (msg: Msg) (model: Neg2To5) : Model =
    match msg with
    | ChangedNeg2To5 newValue ->
        match (intToNeg2To5Option newValue) with
        | Some newNeg2To5 -> newNeg2To5
        | None -> model

open Feliz
open Feliz.Bulma

let view (model: Model) dispatch =
    Bulma.input.number  [
        prop.max 5
        prop.min -2
        prop.value (neg2To5ToInt model)
        prop.onChange ( fun num -> dispatch (ChangedNeg2To5 (num)) )
    ]