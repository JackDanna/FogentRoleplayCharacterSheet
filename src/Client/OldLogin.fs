module OldLogin

open System
open Elmish
open FsToolkit.ErrorHandling
open Feliz.Router
open Shared

type Model = {
    login: Login
    formErrors: string list
}

let init () =
    {
        login = { userName = ""; password = "" }
        formErrors = []
    },
    Cmd.none

type Msg =
    | SetUsername of string
    | SetPassword of string
    | Login
    | LoggedIn of UserData.UserData
    | UnhandledError of exn

let validateUsername name =
    if String.IsNullOrWhiteSpace name then
        Error "You need to fill in a username."
    else
        Ok name

let validatePassword password =
    if String.IsNullOrWhiteSpace password then
        Error "You need to fill in a password."
    else
        Ok password

let validateForm name password = validation {
    let! name = validateUsername name
    and! password = validatePassword password
    return { userName = name; password = password }
}

let update (guestApi: IGuestApi) msg (model: Model) =
    match msg with
    | SetUsername input ->

        {
            model with
                login = { model.login with userName = input }
        },
        Cmd.none
    | SetPassword input ->
        {
            model with
                login = { model.login with password = input }
        },
        Cmd.none
    | Login ->
        let form = validateForm model.login.userName model.login.password

        let model, cmd =
            match form with
            | Ok login ->
                { model with formErrors = [] }, Cmd.OfAsync.either guestApi.login login LoggedIn UnhandledError
            | Error errors -> { model with formErrors = errors }, Cmd.none

        model, cmd
    | LoggedIn user -> model, Cmd.navigate "dashboard" //Cmd.OfFunc.either Session.saveUser user StorageSuccess UnhandledError
    | UnhandledError exn -> model, Cmd.none //exn.AsAlert()

open Feliz
open Feliz.DaisyUI


let view model dispatch =

    Html.div [
        prop.className "grid justify-center"
        prop.children [
            Daisy.card [
                prop.className "shadow-lg grid justify-items-center gap-2 w-[32rem] p-10"
                prop.children [
                    Html.h2 [ prop.text "Log in with 'test' / 'test'." ]
                    model.formErrors
                    |> List.tryHead
                    |> Option.map (fun error -> Html.div [ color.textError; prop.text error ])
                    |> Option.defaultValue (React.fragment [])
                    Daisy.formControl [
                        prop.className ""
                        prop.children [
                            Html.div [
                                prop.className "relative"
                                prop.children [
                                    Html.i [
                                        prop.className
                                            "fa fa-search absolute inset-y-0 end-0 grid items-center mr-2 text-primary"
                                    ]
                                    Daisy.input [
                                        input.bordered
                                        prop.className ""
                                        prop.placeholder "Username"
                                        prop.value model.login.userName
                                        prop.onChange (SetUsername >> dispatch)
                                    ]
                                ]
                            ]
                        ]
                    ]
                    Daisy.formControl [
                        Html.div [
                            prop.className "relative"
                            prop.children [
                                Html.i [
                                    prop.className
                                        "fa fa-lock absolute inset-y-0 end-0 grid items-center mr-2 text-yellow-400"
                                ]
                                Daisy.input [
                                    input.bordered
                                    prop.className ""
                                    prop.placeholder "Password"
                                    prop.value model.login.password
                                    prop.onChange (SetPassword >> dispatch)
                                ]
                            ]
                        ]
                    ]
                    Daisy.formControl [
                        Html.div [
                            prop.className ""
                            prop.children [
                                Daisy.button.button [
                                    button.primary
                                    prop.text "Log In"
                                    prop.onClick (fun _ -> dispatch Login)
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]