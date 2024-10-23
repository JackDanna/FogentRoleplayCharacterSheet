module Neg1To5

open FogentRoleplayLib.Neg1To5

type Msg = ChangedNeg1To5 of int * option<Neg1To5>

let init = FogentRoleplayLib.Neg1To5.init

let update (msg: Msg) (model: Neg1To5) : Neg1To5 =
    match msg with
    | ChangedNeg1To5(newValue, levelCapOption) ->
        match (intToNeg1To5Option newValue), levelCapOption with
        | Some newNeg1To5, Some levelCap ->
            if (neg1To5ToInt newNeg1To5) < (neg1To5ToInt levelCap) then
                levelCap
            else
                model
        | Some newNeg2To5, None -> newNeg2To5
        | _, _ -> model

open Feliz
open ViewUtils

let view (model: Neg1To5) dispatch userInputDisabled =
    numberInput [
        prop.max 5
        prop.min -1
        prop.disabled userInputDisabled
        prop.value (neg1To5ToInt model)
        prop.onChange (fun num -> dispatch (ChangedNeg1To5(num, None)))
    ]