module ItemStack

open FogentRoleplayLib.ItemStack

type Msg = SetItemStackQuantity of uint

let update (msg: Msg) (model: ItemStack) =
    match msg with
    | SetItemStackQuantity msg -> { model with quantity = msg }

open Feliz
open Feliz.Bulma

let view (model: ItemStack) (dispatch: Msg -> unit) =
    let (name, effectNames, weight, value) = Item.view model.item

    name,
    Bulma.input.number [
        prop.min 1
        prop.value (int model.quantity)
        prop.onChange (fun (num: int) -> dispatch (SetItemStackQuantity(uint num)))
    ],
    effectNames,
    weight,
    value