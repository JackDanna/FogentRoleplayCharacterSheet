module Index

open System
open Browser
open Elmish
open Fable.Remoting.Client
//open Feliz.DaisyUI
open Feliz.Router
//open Page
open Shared
open Shared.UserData

type User =
    | Guest
    | User of UserData

type Url =
    | Home
    | Login
    | Overview
    | NotFound
    | Logout

type Page =
    | Home
    | Login of Login.Model
    | Overview of Overview.State
    | NotFound

type Model = { page: Page; url: Url; user: User }

type Msg =
    | LoginMsg of Login.Msg
    | OverviewMsg of Overview.Msg
    | UrlChanged of Url

let parseUrl =
    function
    | [] -> Url.Home
    | [ "login" ] -> Url.Login
    | [ "overview" ] -> Url.Overview
    | [ "logout" ] -> Url.Logout
    | _ -> Url.NotFound

let init () =
    let initialUrl = parseUrl (Router.currentUrl ())

    let defaultState = {
        page = Page.Home
        url = initialUrl
        user = Guest
    }

    match initialUrl with
    | Url.Home -> defaultState, Cmd.none

    | Url.Login ->
        let loginState, loginCmd = Login.init ()

        {
            defaultState with
                page = Page.Login loginState
        },
        Cmd.map LoginMsg loginCmd

    | Url.Overview -> defaultState, Cmd.navigate ("login", HistoryMode.ReplaceState)

    | Url.Logout -> defaultState, Cmd.navigate ("/", HistoryMode.ReplaceState)

    | Url.NotFound ->
        {
            defaultState with
                page = Page.NotFound
        },
        Cmd.none

let fogentRoleplayDataApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IFogentRoleplayDataApi>

let update msg model =
    match msg, model.page with
    | LoginMsg loginMsg, Page.Login loginModel ->
        match loginMsg with
        | Login.UserLoggedIn user -> { model with user = User user }, Cmd.navigate ("/")
        | loginMsg ->
            let loginModel, loginCmd = Login.update loginMsg loginModel

            {
                model with
                    page = Page.Login loginModel
            },
            Cmd.map LoginMsg loginCmd
    | OverviewMsg overviewMsg, Page.Overview overviewModel ->
        let overviewModel, overviewCmd = Overview.update overviewMsg overviewModel

        {
            model with
                page = Page.Overview overviewModel
        },
        Cmd.map OverviewMsg overviewCmd

    | UrlChanged nextUrl, _ ->
        let show page = {
            model with
                page = page
                url = nextUrl
        }

        match nextUrl with
        | Url.Home -> show Page.Home, Cmd.none
        | Url.Login ->
            let login, loginCmd = Login.init ()
            show (Page.Login login), Cmd.map LoginMsg loginCmd

        | Url.Overview ->
            match model.user with
            | Guest -> model, Cmd.navigate ("login", HistoryMode.ReplaceState)
            | User user ->
                let overview, overviewCmd = Overview.init user
                show (Page.Overview overview), Cmd.map OverviewMsg overviewCmd

        | Url.Logout -> { model with user = Guest }, Cmd.navigate ("/")

        | Url.NotFound -> show Page.NotFound, Cmd.none


    | _, _ -> model, Cmd.none

open Feliz
open Feliz.Bulma

let index model dispatch =
    match model.user with
    | Guest ->
        Html.div [
            Html.h1 "Welcome, guest"
            Html.a [
                prop.classes [ "button"; "is-info" ]
                prop.style [ style.margin 5 ]
                prop.href (Router.format ("login"))
                prop.text "Login"
            ]
        ]

    | User user ->
        Html.div [
            Html.h1 (sprintf "Welcome, %s" user.username)
            Html.a [
                prop.classes [ "button"; "is-info" ]
                prop.style [ style.margin 5 ]
                prop.href (Router.format ("overview"))
                prop.text "Overview"
            ]
            Html.a [
                prop.classes [ "button"; "is-info" ]
                prop.style [ style.margin 5 ]
                prop.href (Router.format ("logout"))
                prop.text "Logout"
            ]
        ]

let view model dispatch =
    let activePage =
        match model.page with
        | Page.Login login -> Login.render login (LoginMsg >> dispatch)
        | Page.Overview overview -> Overview.view overview (OverviewMsg >> dispatch)
        | Page.Home -> index model dispatch
        | Page.NotFound -> Html.h1 "Not Found"

    React.router [
        router.onUrlChanged (parseUrl >> UrlChanged >> dispatch)
        router.children [ Html.div [ prop.style [ style.padding 20 ]; prop.children [ activePage ] ] ]

    ]