module DurationAndSource

open FallenLib.TextEffectForDisplay

type Msg =
    | DurationMsg of StringInputTableData.Msg
    | SourceMsg of StringInputTableData.Msg

let update (msg: Msg) (model: DurationAndSource) : DurationAndSource =
    match msg with
    | DurationMsg msg -> { model with duration = StringInputTableData.update msg model.duration }
    | SourceMsg msg -> { model with source = StringInputTableData.update msg model.source }

open Feliz

let nonInteractiveView (model: DurationAndSource) =
    [ Html.td [ prop.text model.duration ]
      Html.td [ prop.text model.source ] ]

let interactiveView (model: DurationAndSource) (dispatch: Msg -> unit) =
    [ StringInputTableData.interactiveView model.duration (DurationMsg >> dispatch)
      StringInputTableData.interactiveView model.source (SourceMsg >> dispatch) ]module DurationAndSource

