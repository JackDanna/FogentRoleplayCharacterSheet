module ItemStack

open FogentRoleplayLib.ItemStack

type Msg = SetItemStackQuantity of uint

let update (msg: Msg) (model: ItemStack) =
    match msg with
    | SetItemStackQuantity msg -> { model with quantity = msg }

open Feliz
open Feliz.Bulma

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