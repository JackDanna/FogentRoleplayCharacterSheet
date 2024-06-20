module NonInteractiveTextEffect

open FogentRoleplayLib.TextEffect

open Feliz

let view (model: TextEffect) =
    [ Html.text model.name; Html.text model.effect ]
    @ DurationAndSource.nonInteractiveView model.durationAndSource