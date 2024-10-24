module CharacterInformation

open FogentRoleplayLib.CharacterInformation

type Msg =
    | SetNotes of string
    | SetAppearance of string
    | SetDisposition of string
    | SetBeliefsAndMorality of string
    | SetGoalsAndAspirations of string
    | SetBackstory of string

let init = FogentRoleplayLib.CharacterInformation.init

let update (msg: Msg) (model: CharacterInformation) =
    match msg with
    | SetNotes newText -> { model with notes = newText }
    | SetAppearance newText -> { model with appearance = newText }
    | SetDisposition newText -> { model with disposition = newText }
    | SetBeliefsAndMorality newText -> {
        model with
            beliefsAndMorality = newText
      }
    | SetGoalsAndAspirations newText -> {
        model with
            goalsAndAspirations = newText
      }
    | SetBackstory newText -> { model with backstory = newText }


open Feliz
open Feliz.DaisyUI

let view (model: CharacterInformation) (dispatch: Msg -> unit) =

    let characterInformationTextArea (labelName: string) (text: string) dispatchMsg =

        Daisy.formControl [
            Daisy.labelText $"{labelName}:"
            Daisy.textarea [
                prop.placeholder $"Enter {labelName}..."
                prop.className "h-24"
                textarea.bordered
                prop.onTextChange (fun text -> dispatch (dispatchMsg text))
            ]
        ]

    Html.div [
        characterInformationTextArea "Backstory" model.backstory SetBackstory
        characterInformationTextArea "Notes" model.notes SetNotes
        characterInformationTextArea "Beliefs/Morality" model.beliefsAndMorality SetBeliefsAndMorality
        characterInformationTextArea "Goals/Aspirations" model.goalsAndAspirations SetGoalsAndAspirations
        characterInformationTextArea "Appearance" model.appearance SetAppearance
        characterInformationTextArea "Disposition" model.disposition SetDisposition
    ]