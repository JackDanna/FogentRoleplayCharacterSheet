module Attribute

open FogentRoleplayLib.Attribute

type Msg = Neg2To5Msg of Neg2To5.Msg

let init = FogentRoleplayLib.Attribute.init

let update msg model =
    match msg with
    | Neg2To5Msg msg -> {
        model with
            level = Neg2To5.update msg model.level
      }

open Feliz
open Feliz.Bulma
open Feliz.DaisyUI

let view model dispatch =

    Html.thead [
        Html.th [ prop.text model.attributeName ]
        Html.th [ Neg2To5.view model.level (Neg2To5Msg >> dispatch) ]
        Html.th []
    ]

let attributeAndCoreSkills model dispatch (preloadedCoreSkillView: Attribute -> ReactElement) =
    Daisy.table [
        view model dispatch
        //
        preloadedCoreSkillView model
    ]