module DestinyPoints

open FogentRoleplayLib.ZeroToThree

type Msg =
    | ToggleOne of bool
    | ToggleTwo of bool
    | ToggleThree of bool

let init = FogentRoleplayLib.ZeroToThree.init

let update msg model =
    match msg with
    | ToggleOne isChecked -> if isChecked then One else Zero
    | ToggleTwo isChecked -> if isChecked then Two else One
    | ToggleThree isChecked -> if isChecked then Three else Two

open Feliz
open Feliz.DaisyUI
open ViewUtils

let isCheckedLogic currentLevel checkboxRepresented =
    zeroToThreeToUint checkboxRepresented <= zeroToThreeToUint currentLevel

let isCheckboxDisabled currentLevel checkboxRepresented =

    let currentLevelInt = zeroToThreeToUint currentLevel
    let checkboxRepresentedInt = zeroToThreeToUint checkboxRepresented

    checkboxRepresentedInt <> currentLevelInt
    && checkboxRepresentedInt <> currentLevelInt + 1u

let view (model: ZeroToThree) dispatch =

    let makeCheckbox specifiedCheckbox toggleLogic =
        Html.div [
            Daisy.checkbox [
                prop.disabled (isCheckboxDisabled model specifiedCheckbox)
                prop.isChecked (isCheckedLogic model specifiedCheckbox)
                prop.onCheckedChange (fun isChecked -> dispatch (toggleLogic isChecked))
            ]
        ]

    Html.div [
        Daisy.labelText "Destiny Points:"
        Daisy.card [
            prop.className "flex flex-row gap-20 p-2"
            prop.children [
                makeCheckbox One ToggleOne
                makeCheckbox Two ToggleTwo
                makeCheckbox Three ToggleThree
            ]
        ]
    ]