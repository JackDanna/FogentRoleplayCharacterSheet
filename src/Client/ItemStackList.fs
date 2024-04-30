module ItemStackList

open FogentRoleplayLib.ItemStack

type Msg =
    | ModifyItemStack of int * ItemStack.Msg
    | Insert of ItemStack
    | Remove of int

let init () : ItemStack list = []

let update (msg: Msg) (model: ItemStack list) : ItemStack list =
    match msg with
    | ModifyItemStack(position, msg) ->
        List.mapi
            (fun index equipment ->
                if position = index then
                    ItemStack.update msg equipment
                else
                    equipment)
            model
    | Insert itemStack -> List.append model [ itemStack ]
    | Remove position -> List.removeAt position model

open Feliz
open Feliz.Bulma

let view (allItemStackList: Map<string, ItemStack>) (model: ItemStack list) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.label "Equipment List:"
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
                        (fun position itemStack ->
                            let itemStackView =
                                ItemStack.view itemStack (fun msg -> dispatch (ModifyItemStack(position, msg)))

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
                        (fun input -> dispatch (Insert <| allItemStackList.Item input))
                        allItemStackList.Keys
                        "ItemStackList"
                ]
            ]
        ]
    ]