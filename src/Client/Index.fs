module Index

open Elmish

open Fable.Remoting.Client
open Shared

open FogentRoleplayLib.Character

type Model = {
    fogentRoleplayData: FogentRoleplayData
    character: Character
}

type Msg =
    | CharacterMsg of Character.Msg
    | GotInitData of FogentRoleplayData

let fogentRoleplayDataApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IFogentRoleplayDataApi>

let init () : Model * Cmd<Msg> =
    let defaultAttributeSet = Set.empty
    let defaultCoreSkillList = Set.empty

    {
        character = Character.init Set.empty
        fogentRoleplayData = {
            attributeNameSet = defaultAttributeSet
            coreSkillDataSet = defaultCoreSkillList
            itemStackMap = Map.empty
            weaponSpellSet = Set.empty
            magicSystemMap = Map.empty
            weaponSkillDataMap = Map.empty
            effectMap = Map.empty
            combatSpeedCalculationMap = Map.empty
        }
    },

    Cmd.OfAsync.perform fogentRoleplayDataApi.getInitData () GotInitData

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | CharacterMsg characterMsg ->

        let temp msg =
            {
                model with
                    character = Character.update msg model.character
            },
            Cmd.none

        match characterMsg with
        | Character.VocationListMsg(VocationList.InsertVocation(x, y, z, _)) ->

            (Character.VocationListMsg(
                VocationList.InsertVocation(x, y, z, Some model.fogentRoleplayData.magicSystemMap)
            ))
            |> temp

        // Check for InsertMundaneVocationSkill
        | Character.VocationListMsg(VocationList.VocationMsgAtPosition(pos1,
                                                                       Vocation.MundaneOrMagicVocationExtrasMsg(MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg(MundaneVocationSkills.InsertMundaneVocationSkill(x,
                                                                                                                                                                                                                       y,
                                                                                                                                                                                                                       _))))) ->
            Character.VocationListMsg(
                VocationList.VocationMsgAtPosition(
                    pos1,
                    Vocation.MundaneOrMagicVocationExtrasMsg(
                        MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg(
                            MundaneVocationSkills.InsertMundaneVocationSkill(
                                x,
                                y,
                                Some model.fogentRoleplayData.weaponSkillDataMap
                            )
                        )
                    )
                )
            )
            |> temp

        // Check for InsertMagicVocationSkill
        | Character.VocationListMsg(VocationList.VocationMsgAtPosition(pos1,
                                                                       Vocation.MundaneOrMagicVocationExtrasMsg(MundaneOrMagicVocationExtras.MagicVocationExtrasMsg(MagicVocationExtras.MagicVocationSkillsMsg(MagicVocationSkills.InsertMagicVocationSkill(x,
                                                                                                                                                                                                                                                            y,
                                                                                                                                                                                                                                                            z,
                                                                                                                                                                                                                                                            _,
                                                                                                                                                                                                                                                            t)))))) ->
            Character.VocationListMsg(
                VocationList.VocationMsgAtPosition(
                    pos1,
                    Vocation.MundaneOrMagicVocationExtrasMsg(
                        MundaneOrMagicVocationExtras.MagicVocationExtrasMsg(
                            MagicVocationExtras.MagicVocationSkillsMsg(
                                MagicVocationSkills.InsertMagicVocationSkill(
                                    x,
                                    y,
                                    z,
                                    Some model.fogentRoleplayData.weaponSkillDataMap,
                                    t
                                )
                            )
                        )
                    )
                )
            )
            |> temp

        | Character.EffectListMsg(EffectList.Insert(effectName, _)) ->

            {
                model with
                    character =
                        Character.update
                            (Character.EffectListMsg(
                                EffectList.Insert(effectName, Some model.fogentRoleplayData.effectMap)
                            ))
                            model.character
            },
            Cmd.none

        | Character.CombatSpeedsMsg(CombatSpeeds.Insert(name, x, y, _)) ->
            {
                model with
                    character =
                        Character.update
                            (Character.CombatSpeedsMsg(
                                CombatSpeeds.Insert(name, x, y, Some model.fogentRoleplayData.combatSpeedCalculationMap)
                            ))
                            model.character
            },
            Cmd.none

        | Character.EquipmentMsg(msg, _) ->
            match msg with
            | ItemStackList.Insert(msg, _) -> ItemStackList.Insert(msg, Some model.fogentRoleplayData.itemStackMap)
            | _ -> msg
            |> (fun msg -> Character.EquipmentMsg(msg, Some model.fogentRoleplayData.weaponSkillDataMap))
            |> (fun msg -> {
                model with
                    character = Character.update msg model.character
            }),
            Cmd.none

        | _ ->
            {
                model with
                    character = Character.update characterMsg model.character
            },
            Cmd.none
    | GotInitData newFogentRoleplayData ->

        {
            model with
                fogentRoleplayData = newFogentRoleplayData
                character = Character.init newFogentRoleplayData.coreSkillDataSet
        },
        Cmd.none

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
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
                Character.view
                    model.fogentRoleplayData.attributeNameSet
                    (model.fogentRoleplayData.itemStackMap.Keys |> Set.ofSeq)
                    (model.fogentRoleplayData.magicSystemMap.Keys |> Set.ofSeq)
                    (model.fogentRoleplayData.weaponSkillDataMap.Keys)
                    (model.fogentRoleplayData.effectMap.Keys |> Set.ofSeq)
                    (model.fogentRoleplayData.combatSpeedCalculationMap.Keys |> Set.ofSeq)
                    model.character
                    (CharacterMsg >> dispatch)
            ]
        ]
    ]