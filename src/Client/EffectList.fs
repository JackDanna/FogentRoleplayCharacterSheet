module EffectList

open FogentRoleplayLib.Effect

type Msg =
    | ModifyEffect of int * Effect.Msg
    | Insert of string * option<Map<string, Effect>>
    | Remove of int

let init () : Effect list = []

let update msg (model: Effect list) =
    match msg with
    | ModifyEffect(pos, effectMsg) ->
        model
        |> List.mapi (fun index effect ->
            if pos = index then
                Effect.update effectMsg effect
            else
                effect)
    | Insert(name, Some effectMap) ->
        match (effectMap.TryFind name) with
        | None -> model
        | Some effect -> List.append model [ effect ]
    | Remove pos -> List.removeAt pos model
    | _ -> model

open Feliz
open Feliz.Bulma

let view (characterEffectNameList: string Set) (model: Effect list) (dispatch: Msg -> unit) =
    Bulma.container [
        Bulma.label "Effects:"
        Bulma.table [
            table.isBordered
            prop.children [
                Html.thead [
                    List.map (fun (thString: string) -> Html.th thString) [ "Name"; "Effect"; "Duration"; "Source" ]
                    |> Html.tr
                ]
                Html.tableBody (
                    List.mapi
                        (fun position equipmentRow ->
                            let characterEffect =
                                (Effect.view equipmentRow (fun msg -> dispatch (ModifyEffect(position, msg))))

                            let deleteEquipmentRowButton =
                                Html.td [
                                    Html.button [ prop.onClick (fun _ -> dispatch (Remove(position))); prop.text "-" ]
                                ]
                                |> List.singleton

                            Html.tr (List.append characterEffect deleteEquipmentRowButton))
                        model
                )
                Html.tfoot [
                    ViewUtils.textInputWithDropdownSet
                        (fun input -> dispatch (Insert(input, None)))
                        characterEffectNameList
                        "EffectList"
                ]
            ]
        ]
    ]