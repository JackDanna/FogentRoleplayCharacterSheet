[<RequireQualifiedAccess>]
module Overview

open Elmish

open Fable.Remoting.Client
open Shared

type Model = {
    User: Shared.UserData
    idCharacterList: IdCharacter seq
    selectedCharacter: int option
}

let getUserApi token =
    Remoting.createApi ()
    |> Remoting.withAuthorizationHeader $"Bearer {token}"
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IUserApi>

type Msg =
    | GotNewIdCharacterList of IdCharacter list
    | SelectCharacter of int
    | DeleteCharacter of int
    | AddNewCharacter
    | CharacterMsg of Character.Msg

let init (user: Shared.UserData) =
    let api = getUserApi user.token

    {
        User = user
        idCharacterList = []
        selectedCharacter = None
    },
    Cmd.OfAsync.perform api.getIdCharacterList user.username GotNewIdCharacterList


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =

    let userApi = getUserApi model.User.token

    match msg with
    | GotNewIdCharacterList idCharacterList ->
        {
            model with
                idCharacterList = idCharacterList
        },
        Cmd.none
    | SelectCharacter index ->
        if Seq.tryFind (fun x -> x.Id = index) model.idCharacterList |> Option.isSome then
            {
                model with
                    selectedCharacter = Some index
            },
            Cmd.none
        else
            model, Cmd.none

    | DeleteCharacter id ->
        {
            model with
                idCharacterList = Seq.filter (fun idCharacter -> idCharacter.Id <> id) model.idCharacterList
        },
        // Need to add deletion in the database
        Cmd.none

    | AddNewCharacter -> model, Cmd.OfAsync.perform userApi.addNewCharacter model.User.username GotNewIdCharacterList

    | CharacterMsg msg ->
        match model.selectedCharacter with
        | None -> model, Cmd.none
        | Some index ->
            let alteredCharacter = {
                model with
                    idCharacterList =
                        model.idCharacterList
                        |> Seq.map (fun idCharacter ->
                            if index = idCharacter.Id then
                                {
                                    idCharacter with
                                        Character = Character.update msg idCharacter.Character
                                }
                            else
                                idCharacter)
            }

            alteredCharacter
            // Need an update function for the database
            ,
            Cmd.none

open Feliz
open Feliz.Bulma

let view (model: Model) dispatch =
    Bulma.hero [
        hero.isFullHeight
        color.isDanger

        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl
                "https://www.onlygfx.com/wp-content/uploads/2015/12/simple-old-paper-1-transparent.jpg"
            style.backgroundPosition "no-repeat center center fixed"
        ]

        prop.children [

            Bulma.heroHead [
                Bulma.navbar [
                    color.isPrimary
                    prop.children [
                        Bulma.navbarItem.div [
                            Bulma.title.h3 [ prop.text "Fallen"; prop.style [ style.fontFamily "PT Serif Caption" ] ]
                        ]
                    ]
                ]
            ]

            Bulma.heroBody [

                Bulma.container [

                    Bulma.container (
                        Seq.map
                            (fun idCharacter ->
                                Bulma.container [
                                    Html.text idCharacter.Character.name
                                    Html.button [
                                        prop.onClick (fun _ -> dispatch (SelectCharacter idCharacter.Id))
                                        prop.text "Select"
                                    ]
                                ])
                            model.idCharacterList
                        |> Seq.append [
                            Html.button [
                                prop.onClick (fun _ -> (dispatch (AddNewCharacter)))
                                prop.text "Add New Character"
                            ]
                        ]
                    )

                    match model.selectedCharacter with
                    | Some index ->
                        Character.view
                            (Seq.find (fun (idCharacter: IdCharacter) -> idCharacter.Id = index) model.idCharacterList)
                                .Character
                            (CharacterMsg >> dispatch)
                    | None -> Html.none
                ]

            ]
        ]
    ]