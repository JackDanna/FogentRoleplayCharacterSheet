module Equipment

open FogentRoleplayLib.Equipment
open FogentRoleplayLib.ItemStack

type Msg =
    | OnPersonContainerInstanceListMsg of ContainerInstanceList.Msg
    | OffPersonContinaerInstacneList of ContainerInstanceList.Msg
    | ItemStackListMsg of ItemStackList.Msg

let init () = {
    itemStackList = ItemStackList.init ()
    onPersonContainerInstanceList = ContainerInstanceList.init ()
    offPersonContinaerInstacneList = ContainerInstanceList.init ()
}

let update msg (model: Equipment) =
    match msg with
    | OnPersonContainerInstanceListMsg msg -> {
        model with
            onPersonContainerInstanceList = ContainerInstanceList.update msg model.onPersonContainerInstanceList
      }
    | OffPersonContinaerInstacneList msg -> {
        model with
            offPersonContinaerInstacneList = ContainerInstanceList.update msg model.offPersonContinaerInstacneList
      }
    | ItemStackListMsg msg ->
        match msg with
        | ItemStackList.Msg.Insert(itemName, Some itemStackMap) ->
            match itemStackMap.TryFind itemName with
            | Some itemStack ->

                {
                    model with
                        itemStackList = ItemStackList.update msg model.itemStackList
                        onPersonContainerInstanceList =
                            itemStack
                            |> itemStackToContinerList
                            |> List.fold
                                (fun acc container ->
                                    ContainerInstanceList.update
                                        (ContainerInstanceList.Insert(itemStack.item.name, container))
                                        acc)
                                model.onPersonContainerInstanceList
                }
            | None -> {
                model with
                    itemStackList = ItemStackList.update msg model.itemStackList
              }
        | ItemStackList.Remove(pos) -> {
            model with
                itemStackList = ItemStackList.update (ItemStackList.Remove(pos)) model.itemStackList
                onPersonContainerInstanceList = List.removeAt pos model.onPersonContainerInstanceList
          }

        | _ -> {
            model with
                itemStackList = ItemStackList.update msg model.itemStackList
          }

open Feliz
open Feliz.Bulma

let view allItemStackNames (model: Equipment) dispatch =
    Bulma.container [
        Bulma.label ("Equiped Items: " + (string (sumItemStackListWeight model.itemStackList)))
        ItemStackList.view allItemStackNames model.itemStackList (ItemStackListMsg >> dispatch)
        Bulma.label "Containers:"
        ContainerInstanceList.view
            allItemStackNames
            model.onPersonContainerInstanceList
            (OnPersonContainerInstanceListMsg >> dispatch)
    ]