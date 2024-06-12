module ContainerInstance

open FogentRoleplayLib.ContainerInstance

type Msg = ItemStackListMsg of ItemStackList.Msg

let update msg model : ContainerInstance =
    match msg with
    | ItemStackListMsg msg -> {
        model with
            itemStackList = ItemStackList.update msg model.itemStackList
      }

open Feliz
open Feliz.Bulma

let view allItemStackNameSet model dispatch =
    Bulma.container [
        Bulma.label model.itemName
        Html.text ("current weight: " + (sumContainerInstanceWeight model |> string))
        Html.text ("max weight: " + (model.containerClass.weightCapacity |> string))
        ItemStackList.view allItemStackNameSet model.itemStackList (ItemStackListMsg >> dispatch)
    ]