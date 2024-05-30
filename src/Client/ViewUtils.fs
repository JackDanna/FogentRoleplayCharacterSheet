module ViewUtils

open Feliz
open Feliz.Bulma
open Browser.Types

let onEnter dispatchOnEnter (ev: KeyboardEvent) =
    let el = ev.target :?> HTMLInputElement

    match ev.key with
    | "Enter" ->
        dispatchOnEnter el.value
        el.value <- ""
    | _ -> ()

let textInputWithDropdownSet (onTextChange: string -> unit) dropdownSet dataListName =
    Html.div [
        Bulma.input.text [ prop.list dataListName; prop.onKeyDown (onEnter onTextChange) ]
        Html.datalist [
            prop.id dataListName
            prop.children (Seq.map (fun (elementName: string) -> Html.option [ prop.value elementName ]) dropdownSet)
        ]
    ]

let deleteEquipmentRowButton onClick =
    Html.td [ Html.button [ prop.onClick onClick; prop.text "-" ] ]
    |> List.singleton