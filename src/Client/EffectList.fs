module EffectList

open FogentRoleplayLib.Effect

let init = FogentRoleplayLib.ListUtils.init

type Msg =
    | ModifyEffect of int * Effect.Msg
    | Insert of string * option<Map<string, Effect>>
    | Remove of int

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

let view
    (characterEffectNameList: string Set)
    (model: Effect list)
    (dispatch: Msg -> unit)
    (weightClassOptionADDME: list<Fable.React.ReactElement>)
    =
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
                            (Effect.view equipmentRow (fun msg -> dispatch (ModifyEffect(position, msg))))
                            @ (Html.button [ prop.onClick (fun _ -> dispatch (Remove(position))); prop.text "-" ]
                               |> Html.td
                               |> List.singleton)
                            |> Html.tr)
                        model
                    |> List.append [ weightClassOptionADDME @ [ Html.none |> Html.td ] |> Html.tr ]
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