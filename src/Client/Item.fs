module Item

open FogentRoleplayLib.Item

type Msg = SetItem of Item

let update (msg: Msg) (model: Item) : Item =
    match msg with
    | SetItem item -> item

open Feliz
open Feliz.Bulma

let itemInput (itemNames: string seq) (itemName: string) onTextChange =
    Html.div [
        Bulma.input.text [
            prop.list "itemNameList"
            prop.value itemName
            prop.onTextChange onTextChange
        ]
        Html.datalist [
            prop.id "itemNameList"
            prop.children (Seq.map (fun (itemName: string) -> Html.option [ prop.value itemName ]) itemNames)
        ]
    ]

let itemRowColumns (itemMap: Map<string, Item>) (model: Item) (dispatch: Msg -> unit) =

    [
        itemInput itemMap.Keys model.name (fun input -> dispatch (SetItem <| itemMap.Item input))
        |> Html.td
        Html.td (itemClassesToString model.itemClasses)
        Html.td $"{model.itemTier.name} ({model.itemTier.level})"
        Html.td model.weight
        Html.td model.value
    ]

let view itemMap (model: Item) (dispatch: Msg -> unit) =
    itemRowColumns itemMap model dispatch |> Html.tr