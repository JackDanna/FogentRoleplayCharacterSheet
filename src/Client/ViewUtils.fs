module ViewUtils

open Feliz
open Feliz.Bulma

let textInputWithDropdownSet onTextChange dropdownSet =
    Html.div [
        Bulma.input.text [ prop.list "allItemStackNames"; prop.onTextChange onTextChange ]
        Html.datalist [
            prop.id "allItemStackNames"
            prop.children (Seq.map (fun (elementName: string) -> Html.option [ prop.value elementName ]) dropdownSet)
        ]
    ]