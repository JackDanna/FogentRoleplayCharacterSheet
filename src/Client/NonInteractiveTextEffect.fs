module NonInteractiveTextEffect

open FogentRoleplayLib.TextEffect

open Feliz

let view (model: TextEffect) =
    [ Html.td [ prop.text model.name ]; Html.td [ prop.text model.effect ] ]
    @ DurationAndSource.nonInteractiveView model.durationAndSource