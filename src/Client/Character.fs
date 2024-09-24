module Character

open FogentRoleplayLib.Character
open FogentRoleplayLib.CoreSkillData
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.Skill
open FogentRoleplayLib.SettingData
open FogentRoleplayLib.ItemElement
open FogentRoleplayLib.CarryWeightCalculation
open FogentRoleplayLib.WeaponSkillData

type Msg =
    | SetSettingData of SettingData
    | SetName of string
    | AttributesMsg of Attributes.Msg
    | CoreSkillsMsg of Skills.Msg
    | DestinyPointMsg of DestinyPoints.Msg
    | VocationListMsg of VocationList.Msg
    | EquipmentMsg of ItemElement.ItemElementListMsgType
    | CharacterInformationMsg of CharacterInformation.Msg
    | EffectListMsg of EffectList.Msg
    | CombatSpeedsMsg of CombatSpeeds.Msg

let init = FogentRoleplayLib.Character.init

let coreSkillToMap (coreSkills: Skill Set) =
    coreSkills |> Set.map (fun x -> x.name, x) |> Map.ofSeq

open Skills
open VocationList
open Vocation

let updateVocationListThenCombatRollList msgs dicePoolCalculation model =
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
                        makeWeaponSkillDataMap model.settingData.weaponSkillDataSet,
                        model.settingData.weaponSpellSet,
                        dicePoolCalculation
                    )
                )
    }

let update msg (model: Character) =

    let dicePoolCalculationData = characterToDicePoolCalculationData model

    let weaponSkillDataMap = makeWeaponSkillDataMap model.settingData.weaponSkillDataSet

    let newEffectsForCharacter character =
        let newDicePoolCalculationData = characterToDicePoolCalculationData character

        let newCoreSkills =
            Skills.update (Skills.CalculateSkillDicePools newDicePoolCalculationData) character.coreSkills

        {
            character with
                coreSkills = newCoreSkills
                combatSpeeds =
                    CombatSpeeds.update
                        (CombatSpeeds.RecalculateAllCombatSpeeds(newCoreSkills, character.attributes))
                        character.combatSpeeds
        }
        |> updateVocationListThenCombatRollList
            [
                (VocationList.CalculateDicePools newDicePoolCalculationData)
                (VocationList.VocationMsgForAll(
                    MundaneOrMagicVocationExtrasMsg(
                        MundaneOrMagicVocationExtras.RecalculateCoreSkillResourcePool(coreSkillToMap newCoreSkills)
                    )
                ))
            ]
            newDicePoolCalculationData

    match msg with
    | SetSettingData newSettingData -> {
        model with
            settingData = newSettingData
      }
    | SetName newName -> { model with name = newName }

    | AttributesMsg msg ->
        {
            model with
                attributes = Attributes.update msg model.attributes
        }
        |> newEffectsForCharacter

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

    | VocationListMsg(msg: VocationList.Msg) ->

        match msg with
        | InsertVocation(vocationName, _, _, _) -> {
            model with
                vocationList =
                    VocationList.update
                        (InsertVocation(
                            vocationName,
                            Some(coreSkillToMap model.coreSkills),
                            Some dicePoolCalculationData,
                            Some model.settingData.magicSystemSet
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
                    | MundaneVocationSkills.InsertMundaneVocationSkill(name, vocationStatLevelOption, _, _) ->
                        MundaneVocationSkills.InsertMundaneVocationSkill(
                            name,
                            vocationStatLevelOption,
                            Some dicePoolCalculationData,
                            Some weaponSkillDataMap
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
                                                                   _,
                                                                   _,
                                                                   _,
                                                                   magicSkillDataMapOption) ->

                        MagicVocationSkills.InsertMagicVocationSkill(
                            name,
                            vocationStatLevelOption,
                            Some model.settingData.attributeNameSet,
                            Some dicePoolCalculationData,
                            Some weaponSkillDataMap,
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
            |> (fun msg -> updateVocationListThenCombatRollList [ msg ] dicePoolCalculationData model)

        | _ -> updateVocationListThenCombatRollList [ msg ] dicePoolCalculationData model
    | EquipmentMsg msg ->
        match msg with
        | ItemElement.ItemElementListMsgType.Insert(itemName, _) ->
            (ItemElement.ItemElementListMsgType.Insert(itemName, Some model.settingData.itemElementSet))

        | ItemElement.ItemElementListMsgType.ModifyItemElement(pos1,
                                                               ItemElement.ItemElementMsgType.ContainerItemMsg(ItemElement.ContainerItemMsgType.ItemElementListMsg(ItemElement.ItemElementListMsgType.Insert(itemName,
                                                                                                                                                                                                             _)))) ->
            (ItemElement.ItemElementListMsgType.ModifyItemElement(
                pos1,
                ItemElement.ItemElementMsgType.ContainerItemMsg(
                    ItemElement.ContainerItemMsgType.ItemElementListMsg(
                        ItemElement.ItemElementListMsgType.Insert(itemName, Some model.settingData.itemElementSet)
                    )
                )
            ))

        | _ -> msg

        |> (fun msg ->
            let newEquipment = ItemElement.itemElementListUpdate msg model.equipment

            {
                model with
                    equipment = newEquipment
                    weightClassOption =
                        WeightClassOption.update
                            (WeightClassOption.DetermineWeightClass(
                                model.carryWeightCalculationOption,
                                model.settingData.weightClassSet,
                                model.attributes,
                                (Skills.update // We recalculate the core skills without the weightClassOption AttributeDeterminedDiceMod since that should only be factored into skill dice pool and not the num dice for determining carry weight
                                    (Skills.CalculateSkillDicePools(
                                        characterToDicePoolCalculationDataWithoutWeightClassOptionEffect model
                                    ))
                                    model.coreSkills),
                                newEquipment
                            ))
                            model.weightClassOption
            })
        |> newEffectsForCharacter

    // | OffPersonContainerInstanceListMsg msg ->
    //     match msg with
    //     | ContainerInstanceList.ModifyContainerInstance(pos1,
    //                                                     ContainerInstance.ItemStackListMsg(ItemStackList.Insert(itemName,
    //                                                                                                             _))) ->
    //         ContainerInstanceList.ModifyContainerInstance(
    //             pos1,
    //             ContainerInstance.ItemStackListMsg(
    //                 ItemStackList.Insert(itemName, Some model.settingData.itemElementMap)
    //             )
    //         )
    //     | _ -> msg
    //     |> (fun msg -> {
    //         model with
    //             offPersonContinaerItemList = ContainerInstanceList.update msg model.offPersonContinaerInstacneList
    //     })

    | CharacterInformationMsg msg -> {
        model with
            characterInformation = CharacterInformation.update msg model.characterInformation
      }

    | EffectListMsg(msg) ->
        match msg with
        | EffectList.Insert(effectName, _) -> EffectList.Insert(effectName, Some model.settingData.effectMap)
        | _ -> msg
        |> (fun msg -> {
            model with
                characterEffects = EffectList.update msg model.characterEffects
        })
        |> newEffectsForCharacter

    | CombatSpeedsMsg msg ->
        match msg with
        | CombatSpeeds.Insert(name, _, _, _) ->
            CombatSpeeds.Insert(
                name,
                Some model.coreSkills,
                Some model.attributes,
                Some model.settingData.combatSpeedCalculationMap
            )
        | _ -> msg
        |> (fun msg -> {
            model with
                combatSpeeds = CombatSpeeds.update msg model.combatSpeeds
        })

open Feliz
open Feliz.Bulma

let view (model: Character) dispatch =

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
        |> Attributes.attributesAndCoreSkillsListView model.attributes (AttributesMsg >> dispatch)

        DestinyPoints.view model.destinyPoints (DestinyPointMsg >> dispatch)

        VocationList.view
            model.settingData.attributeNameSet
            (model.settingData.magicSystemSet |> Seq.map (fun x -> x.name))
            (model.settingData.weaponSkillDataSet |> Set.map (fun x -> x.name))
            model.vocationList
            (VocationListMsg >> dispatch)

        CombatRollList.view model.combatRollList

        CombatSpeeds.view
            (model.settingData.combatSpeedCalculationMap.Keys |> Set.ofSeq)
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
            (model.settingData.effectMap.Keys |> Set.ofSeq)
            model.characterEffects
            (EffectListMsg >> dispatch)

        ItemElement.equipmentView
            (model.settingData.itemElementSet |> Set.map itemElementToName)
            model.equipment
            (EquipmentMsg >> dispatch)

        CharacterInformation.view model.characterInformation (CharacterInformationMsg >> dispatch)
    ]