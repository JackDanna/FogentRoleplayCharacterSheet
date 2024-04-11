module Neg1To5

open FogentRoleplayLib.Neg1To5

type Model = Neg1To5

type Msg = ChangedNeg1To5 of int

let init () = Zero

let update (msg: Msg) (model: Neg1To5) : Model =
    match msg with
    | ChangedNeg1To5 newValue ->
        match (intToNeg1To5Option newValue) with
        | Some newNeg2To5 -> newNeg2To5
        | None -> model

open Feliz
open Feliz.Bulma

let view (model: Model) dispatch isUserInputDisabled =
    Bulma.input.number [
        prop.max 5
        prop.min -1
        prop.disabled isUserInputDisabled
        prop.value (neg1To5ToInt model)
        prop.onChange (fun num -> dispatch (ChangedNeg1To5(num)))
    ]