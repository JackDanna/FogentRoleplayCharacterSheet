module Login

open Elmish
open Feliz
open Fable.Remoting.Client
open Shared

type Model = {
    login: Login
    LoginAttempt: Deferred<Shared.LoginResult>
}

type Msg =
    | UsernameChanged of string
    | PasswordChanged of string
    | Login of AsyncOperationStatus<Shared.LoginResult>

let (|UserLoggedIn|_|) =
    function
    | Msg.Login(Finished(Shared.LoginResult.LoggedIn user)) -> Some user
    | _ -> None

let init () =
    {
        login = { userName = ""; password = "" }
        LoginAttempt = HasNotStartedYet
    },
    Cmd.none

let fogentRoleplayDataApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IFogentRoleplayDataApi>

let update (msg: Msg) (model: Model) =
    match msg with
    | UsernameChanged username ->
        {
            model with
                login = {
                    userName = username
                    password = model.login.password
                }
        },
        Cmd.none

    | PasswordChanged password ->
        {
            model with
                login = {
                    userName = model.login.userName
                    password = password
                }
        },
        Cmd.none

    | Login Started ->
        let nextState = { model with LoginAttempt = InProgress }

        let login = async {
            //let! loginResult = Api.login state.Username state.Password
            let! loginResult = fogentRoleplayDataApi.login model.login
            return Login(Finished loginResult)
        }

        let nextCmd = Cmd.fromAsync login
        nextState, nextCmd

    | Login(Finished loginResult) ->
        let nextState = {
            model with
                LoginAttempt = Resolved loginResult
        }

        nextState, Cmd.none

let renderLoginOutcome (loginResult: Deferred<Shared.LoginResult>) =
    match loginResult with
    | Resolved Shared.LoginResult.UsernameOrPasswordIncorrect ->
        Html.paragraph [
            prop.style [ style.color.crimson; style.padding 10 ]
            prop.text "Username or password is incorrect"
        ]

    | Resolved(Shared.LoginResult.LoggedIn user) ->
        Html.paragraph [
            prop.style [ style.color.green; style.padding 10 ]
            prop.text (sprintf "User '%s' has succesfully logged in" user.username)
        ]

    | otherwise -> Html.none

let layout (children: ReactElement list) =
    Html.section [
        prop.className "hero is-fullheight"
        prop.children [
            Html.div [
                prop.className "hero-body"
                prop.children [
                    Html.div [
                        prop.className "container"
                        prop.children [
                            Html.div [
                                prop.className "columns is-centered"
                                prop.children [
                                    Html.div [
                                        prop.className "column is-6-tablet is-4-desktop is-4-widescreen"
                                        prop.children children
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let centered (children: ReactElement list) =
    Html.div [
        prop.style [ style.margin.auto; style.textAlign.center; style.width (length.percent 100) ]

        prop.children children
    ]

let render (state: Model) (dispatch: Msg -> unit) =
    layout [
        Html.div [
            prop.className "box"
            prop.children [

                centered [
                    Html.img [
                        prop.src "https://fable.io/img/fable_logo.png"
                        prop.height 160
                        prop.width 140
                    ]
                ]

                Html.div [
                    prop.className "field"
                    prop.children [
                        Html.label [ prop.className "label"; prop.text "Username" ]

                        Html.div [
                            prop.className "control has-icons-left"
                            prop.children [
                                Html.input [
                                    prop.className "input"
                                    prop.placeholder "Username"
                                    prop.type'.email
                                    prop.valueOrDefault state.login.userName
                                    prop.onChange (UsernameChanged >> dispatch)
                                ]

                                Html.span [
                                    prop.className "icon is-small is-left"
                                    prop.children [ Html.i [ prop.className "fa fa-user" ] ]
                                ]
                            ]
                        ]
                    ]
                ]

                Html.div [
                    prop.className "field"
                    prop.children [
                        Html.label [ prop.className "label"; prop.text "Password" ]
                        Html.div [
                            prop.className "control has-icons-left"
                            prop.children [
                                Html.input [
                                    prop.className "input"
                                    prop.placeholder "********"
                                    prop.type'.password
                                    prop.valueOrDefault state.login.password
                                    prop.onChange (PasswordChanged >> dispatch)
                                ]
                                Html.span [
                                    prop.className "icon is-small is-left"
                                    prop.children [ Html.i [ prop.className "fa fa-lock" ] ]
                                ]
                            ]
                        ]
                    ]
                ]

                Html.div [
                    prop.className "field"
                    prop.children [
                        Html.button [
                            prop.className [
                                "button is-info is-fullwidth"
                                if state.LoginAttempt = InProgress then
                                    "is-loading"
                            ]

                            prop.onClick (fun _ -> dispatch (Login Started))
                            prop.text "Login"
                        ]
                    ]
                ]

                renderLoginOutcome state.LoginAttempt
            ]
        ]
    ]