module ViewUtils

open Feliz
open Feliz.Bulma

let textInputWithDropdownSet onTextChange dropdownSet dataListName =
    Html.div [
        Bulma.input.text [ prop.list dataListName; prop.onTextChange onTextChange ]
        Html.datalist [
            prop.id dataListName
            prop.children (Seq.map (fun (elementName: string) -> Html.option [ prop.value elementName ]) dropdownSet)
        ]
    ]