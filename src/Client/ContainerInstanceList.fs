module ContainerInstanceList

open FogentRoleplayLib.ContainerInstance
open FogentRoleplayLib.Container

type Msg =
    | ModifyContainerInstance of int * ContainerInstance.Msg
    | Insert of string * Container
    | Remove of int

let init () = []

let update msg (model: ContainerInstance List) =
    match msg with
    | ModifyContainerInstance(pos, msg) ->
        model
        |> List.mapi (fun index containerInstance ->
            if pos = index then
                ContainerInstance.update msg containerInstance
            else
                containerInstance)
    | Insert(itemName, container) -> List.append model [ initContainerInstance (itemName, container) ]
    | Remove position -> List.removeAt position model


open Feliz
open Feliz.Bulma

let view allItemStackNameSet model dispatch =
    Bulma.container (
        model
        |> List.mapi (fun position constainerInstance ->
            ContainerInstance.view allItemStackNameSet constainerInstance (fun msg ->
                ModifyContainerInstance(position, msg) |> dispatch))
    )