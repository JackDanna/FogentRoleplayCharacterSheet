module InteractiveTextEffect

open FogentRoleplayLib.TextEffect

type Msg =
    | NameMsg of StringInput.Msg
    | EffectMsg of StringInput.Msg
    | DurationAndSourceMsg of DurationAndSource.Msg

let update (msg: Msg) (model: TextEffect) : TextEffect =
    match msg with
    | NameMsg msg -> {
        model with
            name = StringInput.update msg model.name
      }
    | EffectMsg msg -> {
        model with
            effect = StringInput.update msg model.effect
      }
    | DurationAndSourceMsg msg -> {
        model with
            durationAndSource = DurationAndSource.update msg model.durationAndSource
      }

open Feliz

let view (model: TextEffect) (dispatch: Msg -> unit) =
    [
        StringInput.view model.name (NameMsg >> dispatch)
        StringInput.view model.effect (EffectMsg >> dispatch)
    ]
    @ DurationAndSource.interactiveView model.durationAndSource (DurationAndSourceMsg >> dispatch)