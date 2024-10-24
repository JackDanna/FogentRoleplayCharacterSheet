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

let numberInput props =
    [ prop.type' "number" ] @ props |> Daisy.input

let deleteEquipmentRowButton onClick =
    Html.td [ Html.button [ prop.onClick onClick; prop.text "-" ] ]
    |> List.singleton

let horizontalDiv props =
    [ prop.className "flex flex-row w-full gap-4" ] @ props |> Html.div