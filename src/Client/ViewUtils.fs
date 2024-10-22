module ViewUtils

open Browser.Types

let onEnter dispatchOnEnter (keyboardEvent: KeyboardEvent) =
    let el = keyboardEvent.target :?> HTMLInputElement

    match keyboardEvent.key with
    | "Enter" ->
        dispatchOnEnter el.value
        el.value <- ""
    | _ -> ()

open Feliz
open Feliz.DaisyUI

let textInput list =
    [ prop.type' "text" ] @ list |> Daisy.input

let textInputWithDropdownSet (onTextChange: string -> unit) dropdownSet dataListName =
    Html.div [
        textInput [ prop.list dataListName; prop.onKeyDown (onEnter onTextChange) ]
        Html.datalist [
            prop.id dataListName
            prop.children (Seq.map (fun (elementName: string) -> Html.option [ prop.value elementName ]) dropdownSet)
        ]
    ]

let deleteEquipmentRowButton onClick =
    Html.td [ Html.button [ prop.onClick onClick; prop.text "-" ] ]
    |> List.singleton