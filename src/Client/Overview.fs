[<RequireQualifiedAccess>]
module Overview

open Elmish
open Feliz

type State = { User: Shared.UserData.UserData }

type Msg = Msg of unit

let init (user: Shared.UserData.UserData) = { User = user }, Cmd.none

let update (msg: Msg) (state: State) : State * Cmd<Msg> = state, Cmd.none

let centered (children: ReactElement list) =
    Html.div [
        prop.style [
            style.margin.auto
            style.textAlign.center
            style.padding 20
            style.width (length.percent 100)
        ]

        prop.children children
    ]

let render (state: State) (dispatch: Msg -> unit) =
    centered [
        Html.h1 [ Html.strong (state.User.username.ToUpper()) ]

        Html.p "This is the overview page"
    ]