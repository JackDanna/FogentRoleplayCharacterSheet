module Character

open FogentRoleplayLib
open Character
open Skill
open SettingData
open ItemElement
open CarryWeightCalculation
open WeaponSkillData
open Effect
open CombatSpeedCalculation

type Msg =
    | SetName of string
    | AttributesMsg of Attributes.Msg * Option<SettingData>
    | CoreSkillsMsg of Skills.Msg
    | DestinyPointMsg of DestinyPoints.Msg
    | VocationListMsg of VocationList.Msg * Option<SettingData>
    | EquipmentMsg of ItemElement.ItemElementListMsgType * Option<SettingData>
    | CharacterInformationMsg of CharacterInformation.Msg
    | EffectListMsg of EffectList.Msg * Option<SettingData>
    | CombatSpeedsMsg of CombatSpeeds.Msg

let init = FogentRoleplayLib.Character.init

let coreSkillToMap (coreSkills: Skill Set) =
    coreSkills |> Set.map (fun x -> x.name, x) |> Map.ofSeq

open Skills
open VocationList
open Vocation

let updateVocationListThenCombatRollList msgs dicePoolCalculation model tempSettingData =
    let newVocationList =
        msgs
        |> List.fold (fun acc msg -> VocationList.update msg acc) model.vocationList

    {
        model with
            vocationList = newVocationList
            combatRollList =
                CombatRollList.update (
                    CombatRollList.RecalculateCombatRollList(
                        model.equipment,
                        newVocationList,
                        makeWeaponSkillDataMap tempSettingData.weaponSkillDataSet,
                        tempSettingData.weaponSpellSet,
                        dicePoolCalculation
                    )
                )
    }

let newEffectsForCharacter settingData character =
    let newDicePoolCalculationData = characterToDicePoolCalculationData character

    let newCoreSkills =
        Skills.update (Skills.CalculateSkillDicePools newDicePoolCalculationData) character.coreSkills


    updateVocationListThenCombatRollList
        [
            (VocationList.CalculateDicePools newDicePoolCalculationData)
            (VocationList.VocationMsgForAll(
                MundaneOrMagicVocationExtrasMsg(
                    MundaneOrMagicVocationExtras.RecalculateCoreSkillResourcePool(coreSkillToMap newCoreSkills)
                )
            ))
        ]
        newDicePoolCalculationData
        {
            character with
                coreSkills = newCoreSkills
                combatSpeeds =
                    CombatSpeeds.update
                        (CombatSpeeds.RecalculateAllCombatSpeeds(newCoreSkills, character.attributes))
                        character.combatSpeeds
        }
        settingData

let update msg (model: Character) =

    let dicePoolCalculationData = characterToDicePoolCalculationData model

    match msg with
    | SetName newName -> { model with name = newName }

    | AttributesMsg(msg, Some settingData) ->
        {
            model with
                attributes = Attributes.update msg model.attributes
        }
        |> newEffectsForCharacter settingData

    | CoreSkillsMsg msg ->
        match msg with
        | ModifySkillAtPosition(pos, Skill.ModifySkillLevel(msg, levelCapOption, _)) ->
            Skills.update
                (ModifySkillAtPosition(pos, Skill.ModifySkillLevel(msg, levelCapOption, Some dicePoolCalculationData)))
                model.coreSkills

        | _ -> Skills.update msg model.coreSkills

        |> (fun newCoreSkills -> {
            model with
                coreSkills = newCoreSkills
                combatSpeeds =
                    CombatSpeeds.update
                        (CombatSpeeds.RecalculateAllCombatSpeeds(newCoreSkills, model.attributes))
                        model.combatSpeeds
                vocationList =
                    VocationList.update
                        (VocationMsgForAll(
                            MundaneOrMagicVocationExtrasMsg(
                                MundaneOrMagicVocationExtras.RecalculateCoreSkillResourcePool(
                                    coreSkillToMap newCoreSkills
                                )
                            )
                        ))
                        model.vocationList
        })

    | DestinyPointMsg msg -> {
        model with
            destinyPoints = DestinyPoints.update msg model.destinyPoints
      }

    | VocationListMsg(msg, Some settingData) ->

        match msg with
        | InsertVocation(vocationName, _, _, magicSystemsOption) -> {
            model with
                vocationList =
                    VocationList.update
                        (InsertVocation(
                            vocationName,
                            Some(coreSkillToMap model.coreSkills),
                            Some dicePoolCalculationData,
                            magicSystemsOption
                        ))
                        model.vocationList
          }
        | VocationMsgAtPosition(pos1, msg) ->
            match msg with
            | VocationStatMsg msg ->

                match msg with
                | VocationStat.ToggleGoveringAttribute(msg, _) ->
                    VocationStat.ToggleGoveringAttribute(msg, Some dicePoolCalculationData)
                | VocationStat.ZeroToFiveMsg(msg, _) -> VocationStat.ZeroToFiveMsg(msg, Some dicePoolCalculationData)
                | _ -> msg
                |> VocationStatMsg

            | Vocation.MundaneOrMagicVocationExtrasMsg(msg) ->
                match msg with
                | MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg(msg) ->
                    match msg with
                    | MundaneVocationSkills.InsertMundaneVocationSkill(name,
                                                                       vocationStatLevelOption,
                                                                       _,
                                                                       weaponSkillDataOption) ->
                        MundaneVocationSkills.InsertMundaneVocationSkill(
                            name,
                            vocationStatLevelOption,
                            Some dicePoolCalculationData,
                            weaponSkillDataOption
                        )

                    | MundaneVocationSkills.ModifyMundaneVocationSkillAtPosition(pos2,
                                                                                 MundaneVocationSkill.SkillMsg(msg)) ->

                        match msg with
                        | Skill.ModifySkillLevel(msg, zeroToFiveOption, _) ->
                            Skill.ModifySkillLevel(msg, zeroToFiveOption, Some dicePoolCalculationData)
                        | Skill.ToggleGoverningAttribute(attributeName, _) ->
                            Skill.ToggleGoverningAttribute(attributeName, Some dicePoolCalculationData)
                        | _ -> msg
                        |> MundaneVocationSkill.SkillMsg
                        |> (fun msg -> MundaneVocationSkills.ModifyMundaneVocationSkillAtPosition(pos2, msg))

                    | _ -> msg
                    |> MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg

                | MundaneOrMagicVocationExtras.MagicVocationExtrasMsg(MagicVocationExtras.MagicVocationSkillsMsg msg) ->
                    match msg with
                    | MagicVocationSkills.InsertMagicVocationSkill(name,
                                                                   vocationStatLevelOption,
                                                                   attributeNameSetOption,
                                                                   _,
                                                                   weaponSkillDataMapOption,
                                                                   magicSkillDataMapOption) ->

                        MagicVocationSkills.InsertMagicVocationSkill(
                            name,
                            vocationStatLevelOption,
                            attributeNameSetOption,
                            Some dicePoolCalculationData,
                            weaponSkillDataMapOption,
                            magicSkillDataMapOption
                        )

                    | MagicVocationSkills.ModifySkillAtPosition(pos2, msg) ->

                        match msg with
                        | MagicVocationSkill.MagicSkillMsg(msg) ->

                            match msg with
                            | Skill.ModifySkillLevel(msg, zeroToFiveOption, _) ->
                                Skill.ModifySkillLevel(msg, zeroToFiveOption, Some dicePoolCalculationData)
                            | Skill.ToggleGoverningAttribute(attributeName, _) ->
                                Skill.ToggleGoverningAttribute(attributeName, Some dicePoolCalculationData)
                            | _ -> msg
                            |> MagicVocationSkill.MagicSkillMsg

                        | MagicVocationSkill.MundaneVocationSkillMsg(MundaneVocationSkill.SkillMsg(msg)) ->
                            match msg with
                            | Skill.ModifySkillLevel(msg, zeroToFiveOption, _) ->
                                Skill.ModifySkillLevel(msg, zeroToFiveOption, Some dicePoolCalculationData)
                            | Skill.ToggleGoverningAttribute(attributeName, _) ->
                                Skill.ToggleGoverningAttribute(attributeName, Some dicePoolCalculationData)
                            | _ -> msg
                            |> MundaneVocationSkill.SkillMsg
                            |> MagicVocationSkill.MundaneVocationSkillMsg
                        | _ -> msg
                        |> (fun msg -> MagicVocationSkills.ModifySkillAtPosition(pos2, msg))

                    | _ -> msg
                    |> MagicVocationExtras.MagicVocationSkillsMsg
                    |> MundaneOrMagicVocationExtras.MagicVocationExtrasMsg

                | _ -> msg
                |> Vocation.MundaneOrMagicVocationExtrasMsg

            | _ -> msg
            |> (fun msg -> VocationMsgAtPosition(pos1, msg))
            |> (fun msg -> updateVocationListThenCombatRollList [ msg ] dicePoolCalculationData model settingData)

        | _ -> updateVocationListThenCombatRollList [ msg ] dicePoolCalculationData model settingData
    | EquipmentMsg(msg, Some settingData) ->

        let newEquipment = ItemElement.itemElementListUpdate msg model.equipment


        {
            model with
                equipment = newEquipment
                weightClassOption =
                    WeightClassOption.update
                        (WeightClassOption.DetermineWeightClass(
                            model.carryWeightCalculationOption,
                            settingData.weightClassSet,
                            model.attributes,
                            (Skills.update // We recalculate the core skills without the weightClassOption AttributeDeterminedDiceMod since that should only be factored into skill dice pool and not the num dice for determining carry weight
                                (Skills.CalculateSkillDicePools(
                                    characterToDicePoolCalculationDataWithoutWeightClassOptionEffect model
                                ))
                                model.coreSkills),
                            newEquipment
                        ))
                        model.weightClassOption
        }
        |> newEffectsForCharacter settingData

    | CharacterInformationMsg msg -> {
        model with
            characterInformation = CharacterInformation.update msg model.characterInformation
      }

    | EffectListMsg(msg, Some settingData) ->
        {
            model with
                characterEffects = EffectList.update msg model.characterEffects
        }
        |> newEffectsForCharacter settingData

    | CombatSpeedsMsg msg ->
        match msg with
        | CombatSpeeds.Insert(name, _, _, settingDataOption) ->
            CombatSpeeds.Insert(name, Some model.coreSkills, Some model.attributes, settingDataOption)
        | _ -> msg
        |> (fun msg -> {
            model with
                combatSpeeds = CombatSpeeds.update msg model.combatSpeeds
        })

    | _ -> model

open Feliz
open Feliz.Bulma

let view (model: Character) dispatch settingData =

    Bulma.container [

        Bulma.input.text [
            prop.value model.name
            prop.placeholder "Character Name"
            prop.onTextChange (SetName >> dispatch)
            prop.classes [ "is-large"; "has-text-centered" ]
        ]
        |> Bulma.content

        Bulma.image [
            Html.img [
                prop.style [ style.height 500; style.width 500 ]
                prop.classes [ "center" ]

                prop.src "https://cogentroleplaycommunity.github.io/Fallen/src/Characters/PC/JavkWick/Javk-Wick.png"
            ]
        ]
        |> Bulma.content

        Skills.coreSkillsView model.coreSkills (CoreSkillsMsg >> dispatch)
        |> Attributes.attributesAndCoreSkillsListView model.attributes ((fun x -> AttributesMsg(x, None)) >> dispatch)

        DestinyPoints.view model.destinyPoints (DestinyPointMsg >> dispatch)

        VocationList.view
            settingData.attributeNameSet
            (settingData.magicSystemSet |> Seq.map (fun x -> x.name))
            (settingData.weaponSkillDataSet |> Set.map (fun x -> x.name))
            model.vocationList
            ((fun msg -> VocationListMsg(msg, None)) >> dispatch)

        CombatRollList.view model.combatRollList

        CombatSpeeds.view
            (settingData.combatSpeedCalculationSet |> Set.map (fun x -> x.name))
            model.combatSpeeds
            (CombatSpeedsMsg >> dispatch)

        match model.carryWeightCalculationOption with
        | Some carryWeightCalculation ->
            WeightClassOption.view
                model.weightClassOption
                (sumItemElementListWeight model.equipment)
                (calculateCarryWeight (carryWeightCalculation) model.attributes model.coreSkills)
        | None -> []

        |> EffectList.view
            (settingData.effectSet |> Set.map effectToEffectName)
            model.characterEffects
            ((fun msg -> EffectListMsg(msg, None)) >> dispatch)

        ItemElement.equipmentView
            (settingData.itemElementSet |> Set.map itemElementToName)
            model.equipment
            ((fun msg -> EquipmentMsg(msg, None)) >> dispatch)

        CharacterInformation.view model.characterInformation (CharacterInformationMsg >> dispatch)
    ]