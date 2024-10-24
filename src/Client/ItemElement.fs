module ItemElement

open FogentRoleplayLib.ItemElement

// Inits

let init = FogentRoleplayLib.ListUtils.init

// Msgs

type ContainerItemMsgType = ItemElementListMsg of ItemElementListMsgType

and ItemElementMsgType =
    | ItemStackMsg of ItemStack.Msg
    | ContainerItemMsg of ContainerItemMsgType

and ItemElementListMsgType =
    | ModifyItemElement of int * ItemElementMsgType
    | Insert of string * option<ItemElement Set>
    | Remove of int

// Updates

let rec containerItemUpdate msg model =
    match msg with
    | ItemElementListMsg msg -> {
        model with
            containedElements = itemElementListUpdate msg model.containedElements
      }

and itemElementUpdate (msg: ItemElementMsgType) (model: ItemElement) : ItemElement =
    match msg, model with
    | ItemStackMsg msg, ItemStack itemStack -> ItemStack.update msg itemStack |> ItemStack
    | ContainerItemMsg msg, ContainerItem containerItem -> containerItemUpdate msg containerItem |> ContainerItem
    | _, _ -> model

and itemElementListUpdate msg (model: ItemElement list) : ItemElement list =
    match msg with
    | ModifyItemElement(position, msg) ->
        List.mapi
            (fun index equipment ->
                if position = index then
                    itemElementUpdate msg equipment
                else
                    equipment)
            model
    | Insert(itemElementName: string, Some itemElementSet) ->
        match tryFindItemElement itemElementSet itemElementName with
        | Some itemElement -> List.append model [ itemElement ]
        | None -> model
    | Remove position -> List.removeAt position model
    | _ -> model

// Views

open Feliz
open Feliz.Bulma

let containerItemView (model: ContainerItem) =
    let (name, effectsName, weight, value) = Item.view model.item
    name, Html.text model.containerTypeData.name, effectsName, weight, value

let itemElementView model dispatch =
    match model with
    | Item item ->
        let (name, effectsName, weight, value) = Item.view item

        name, None, effectsName, weight, value
    | ItemStack itemStack ->
        let (name, quantityUI, effectsName, weight, value) =
            ItemStack.view itemStack (ItemStackMsg >> dispatch)

        name, Some quantityUI, effectsName, weight, value
    | ContainerItem containerItem ->
        let (name, containerNameUI, effectsName, weight, value) =
            containerItemView containerItem

        name, Some containerNameUI, effectsName, weight, value
    |> (fun (name, quantityOrContainerUIOption, effectsName, weight, value) -> [
        Html.td name
        match quantityOrContainerUIOption with
        | None -> Html.td []
        | Some quantityOrContainer -> Html.td quantityOrContainer
        Html.td effectsName
        Html.td weight
        Html.td value

    ])

let itemElementListView (allItemStackNameSet: string Set) (model: ItemElement list) dispatch =
    Bulma.container [
        Bulma.table [
            table.isBordered
            prop.children [
                Html.thead [
                    List.map (fun (thString: string) -> Html.th thString) [ "Name"; "#"; "Effect"; "LB"; "Value" ]
                    |> Html.tr
                ]
                Html.tableBody (
                    List.mapi
                        (fun position itemElement ->
                            let itemElementView =
                                itemElementView itemElement (fun msg -> dispatch (ModifyItemElement(position, msg)))

                            let deleteEquipmentRowButton =
                                Html.td [
                                    Html.button [ prop.onClick (fun _ -> dispatch (Remove(position))); prop.text "-" ]
                                ]
                                |> List.singleton

                            Html.tr (List.append itemElementView deleteEquipmentRowButton))
                        model
                )
                Html.tfoot [
                    ViewUtils.textInputWithDropdownSet
                        (fun input -> dispatch (Insert(input, None)))
                        allItemStackNameSet
                        "ItemStackList"
                ]
            ]
        ]
    ]

// Views for container Items

let containerTypeAndContainedItemView allItemStackNameSet (model: ContainerItem) dispatch =
    Bulma.container [
        Bulma.label model.item.name
        Html.text (
            "current weight: "
            + (sumItemElementListWeight model.containedElements |> string)
        )
        Html.text ("max weight: " + (model.containerTypeData.weightCapacity |> string))
        itemElementListView allItemStackNameSet model.containedElements (ItemElementListMsg >> dispatch)
    ]

let containerItemListView allItemStackNameSet (model: ItemElement list) (dispatch: ItemElementListMsgType -> unit) =
    Bulma.container (
        model
        |> List.mapi (fun position itemElement ->
            match itemElement with
            | ContainerItem containerItem ->

                containerTypeAndContainedItemView
                    allItemStackNameSet
                    containerItem
                    (ContainerItemMsg >> (fun msg -> ModifyItemElement(position, msg)) >> dispatch)
            | _ -> Html.none)
    )

// Equipment view

let equipmentView allItemStackNames (model: ItemElement list) (dispatch: ItemElementListMsgType -> unit) =
    Bulma.container [
        Bulma.label "Equiped Items:"
        itemElementListView allItemStackNames model dispatch
        Bulma.label "Containers:"
        containerItemListView allItemStackNames model dispatch
    ]