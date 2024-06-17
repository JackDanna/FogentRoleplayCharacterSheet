module ItemElementList

open FogentRoleplayLib.ItemElement

type Msg =
    | ModifyItemStack of int * ItemElement.Msg
    | Insert of string * option<Map<string, ItemElement>>
    | Remove of int

let init () : ItemElement list = []

let update (msg: Msg) (model: ItemElement list) : ItemElement list =
    match msg with
    | ModifyItemStack(position, msg) ->
        List.mapi
            (fun index equipment ->
                if position = index then
                    ItemElement.update msg equipment
                else
                    equipment)
            model
    | Insert(itemStackName, Some itemStackMap) ->
        match itemStackMap.TryFind itemStackName with
        | Some itemStack -> List.append model [ itemStack ]
        | None -> model
    | Remove position -> List.removeAt position model
    | _ -> model

open Feliz
open Feliz.Bulma

let view (allItemStackNameSet: string Set) (model: ItemElement list) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.table [
            table.isBordered
            prop.children [
                Html.thead [
                    List.map (fun (thString: string) -> Html.th thString) [
                        "Name"
                        "#"
                        "Effect"
                        "Tier"
                        "LB"
                        "Value"
                    ]
                    |> Html.tr
                ]
                Html.tableBody (
                    List.mapi
                        (fun position itemElement ->
                            let itemStackView =
                                ItemElement.view itemElement (fun msg -> dispatch (ModifyItemStack(position, msg)))

                            let deleteEquipmentRowButton =
                                Html.td [
                                    Html.button [ prop.onClick (fun _ -> dispatch (Remove(position))); prop.text "-" ]
                                ]
                                |> List.singleton

                            Html.tr (List.append itemStackView deleteEquipmentRowButton))
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