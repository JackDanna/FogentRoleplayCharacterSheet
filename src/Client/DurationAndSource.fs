module DurationAndSource

open FogentRoleplayLib.DurationAndSource

type Msg =
    | DurationMsg of StringInput.Msg
    | SourceMsg of StringInput.Msg

let update (msg: Msg) (model: DurationAndSource) : DurationAndSource =
    match msg with
    | DurationMsg msg -> {
        model with
            duration = StringInput.update msg model.duration
      }
    | SourceMsg msg -> {
        model with
            source = StringInput.update msg model.source
      }

open Feliz

let nonInteractiveView (model: DurationAndSource) = [
    Html.td [ prop.text model.duration ]
    Html.td [ prop.text model.source ]
]

let interactiveView (model: DurationAndSource) (dispatch: Msg -> unit) = [
    StringInput.view model.duration (DurationMsg >> dispatch)
    StringInput.view model.source (SourceMsg >> dispatch)
]