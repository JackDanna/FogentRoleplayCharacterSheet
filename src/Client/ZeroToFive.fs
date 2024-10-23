module ZeroToFive

open FogentRoleplayLib.ZeroToFive

type Msg = ChangedZeroToFive of int

let init () = Zero

let update msg model =
    match msg with
    | ChangedZeroToFive newValue ->
        match (intToZeroToFiveOption newValue) with
        | Some newZeroToFive -> newZeroToFive
        | None -> model

open Feliz
open ViewUtils

let view (model: ZeroToFive) dispatch =
    numberInput [
        prop.max 5
        prop.min 0
        prop.value (zeroToFiveToUint model |> int)
        prop.onChange (fun num -> dispatch (ChangedZeroToFive(num)))
    ]