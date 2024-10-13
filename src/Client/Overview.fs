[<RequireQualifiedAccess>]
module Overview

open Elmish

open Fable.Remoting.Client
open Shared
open FogentRoleplayLib.Character

type Model = {
    User: Shared.UserData
    characterList: Character seq
    selectedCharacter: int option
}

let getUserApi token =
    Remoting.createApi ()
    |> Remoting.withAuthorizationHeader $"Bearer {token}"
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IUserApi>

type Msg =
    | GotNewIdCharacterList of Character list
    | SelectCharacter of int
    | DeleteCharacter of int
    | AddNewCharacter
    | CharacterMsg of Character.Msg
    | UpdatedIdCharacterOnDB of unit

let init (user: Shared.UserData) =
    let api = getUserApi user.token

    {
        User = user
        characterList = []
        selectedCharacter = None
    },
    Cmd.OfAsync.perform api.getIdCharacterList user.username GotNewIdCharacterList


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =

    let userApi = getUserApi model.User.token

    match msg with
    | GotNewIdCharacterList idCharacterList ->
        {
            model with
                characterList = idCharacterList
        },
        Cmd.none
    | SelectCharacter index ->
        if Seq.tryFind (fun x -> x.id = index) model.characterList |> Option.isSome then
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
                characterList = Seq.filter (fun idCharacter -> idCharacter.id <> id) model.characterList
        },
        // Need to add deletion in the database
        Cmd.none

    | AddNewCharacter -> model, Cmd.OfAsync.perform userApi.addNewCharacter model.User.username GotNewIdCharacterList

    | CharacterMsg msg ->
        match model.selectedCharacter with
        | None -> model, Cmd.none
        | Some index ->

            model.characterList
            |> Seq.tryFind (fun x -> x.id = index)
            |> function
                | None -> model, Cmd.none
                | Some idCharacter ->
                    let updatedIdCharacter = Character.update msg idCharacter

                    {
                        model with
                            characterList =
                                Seq.map
                                    (fun x ->
                                        if x.id = updatedIdCharacter.id then
                                            updatedIdCharacter
                                        else
                                            x)
                                    model.characterList
                    },
                    Cmd.OfAsync.perform
                        (userApi.updateIdCharacter model.User.username)
                        updatedIdCharacter
                        UpdatedIdCharacterOnDB

    | UpdatedIdCharacterOnDB _ -> model, Cmd.none

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
                            (fun character ->
                                Bulma.container [
                                    Html.text character.name
                                    Html.button [
                                        prop.onClick (fun _ -> dispatch (SelectCharacter character.id))
                                        prop.text "Select"
                                    ]
                                ])
                            model.characterList
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
                            (Seq.find (fun (character: Character) -> character.id = index) model.characterList)
                            (CharacterMsg >> dispatch)
                    | None -> Html.none
                ]

            ]
        ]
    ]