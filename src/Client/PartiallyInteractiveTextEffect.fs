module PartiallyInteractiveTextEffect

open FogentRoleplayLib.TextEffect

type Msg = DurationAndSourceMsg of DurationAndSource.Msg

let update (msg: Msg) (model: TextEffect) : TextEffect =
    match msg with
    | DurationAndSourceMsg msg -> {
        model with
            durationAndSource = DurationAndSource.update msg model.durationAndSource
      }

open Feliz

let view (model: TextEffect) (dispatch: Msg -> unit) =
    [ Html.text model.name; Html.text model.effect ]
    @ DurationAndSource.interactiveView model.durationAndSource (DurationAndSourceMsg >> dispatch)