module ItemStack

open FogentRoleplayLib.ItemStack

type Msg =
    //| ItemMsg of Item.Msg
    | SetItemStackQuantity of uint

let update (msg: Msg) (model: ItemStack) =
    match msg with
    // | ItemMsg msg -> {
    //     model with
    //         item = Item.update msg model.item
    //   }
    | SetItemStackQuantity msg -> { model with quantity = msg }

open Feliz
open Feliz.Bulma

// let itemStackRowTableData (itemNameList: ItemStack list) (model: ItemStack) (dispatch: Msg -> unit) =
//     List.append
//         [
//             (Html.td [
//                 Bulma.input.number [
//                     prop.min 0
//                     prop.value (int model.quantity)
//                     prop.onChange (fun (num: int) -> dispatch (SetItemStackQuantity(uint num)))
//                 ]
//             ])
//         ]
//         (Item.view itemNameList model.item (ItemRowMsg >> dispatch))

let view (model: ItemStack) (dispatch: Msg -> unit) =

    List.insertAt
        1
        (Html.td [
            Bulma.input.number [
                prop.min 1
                prop.value (int model.quantity)
                prop.onChange (fun (num: int) -> dispatch (SetItemStackQuantity(uint num)))
            ]
        ])
        (Item.view model.item)