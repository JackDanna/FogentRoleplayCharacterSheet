module StringInput

type Msg = SetString of string

let init () : string = ""

let update (msg: Msg) (model: string) : string =
    match msg with
    | SetString newName -> newName

open Feliz
open Feliz.Bulma

let view (model: string) (dispatch: Msg -> unit) =
    Bulma.input.text [
        prop.value model
        prop.onTextChange (fun input -> dispatch (SetString input))
    ]