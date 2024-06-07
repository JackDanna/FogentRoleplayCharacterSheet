module Character

open FogentRoleplayLib.Character
open FogentRoleplayLib.WeaponSkillData
open FogentRoleplayLib.SkillName
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.CoreSkillData
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.Skill
open FogentRoleplayLib.SettingData

type Msg =
    | SetSettingData of SettingData
    | SetName of string
    | AttributesMsg of Attributes.Msg
    | CoreSkillsMsg of Skills.Msg
    | VocationListMsg of VocationList.Msg
    | EquipmentMsg of ItemStackList.Msg
    | CharacterInformationMsg of CharacterInformation.Msg
    | EffectListMsg of EffectList.Msg
    | CombatSpeedsMsg of CombatSpeeds.Msg

let init (settingData: SettingData) =
    let attributes =
        Set.map (fun x -> Attribute.init x.attributeName) settingData.coreSkillDataSet

    let effects = EffectList.init ()

    let dicePoolCalculationData: DicePoolCalculationData = {
        effects = effects
        attributes = attributes
    }

    {
        name = ""
        attributes = attributes
        coreSkills = Skills.initCoreSkills settingData.coreSkillDataSet dicePoolCalculationData
        vocationList = VocationList.init ()
        equipmentList = ItemStackList.init ()
        combatRollList = CombatRollList.init ()
        characterInformation = CharacterInformation.init ()
        characterEffects = effects
        combatSpeeds = CombatSpeeds.init ()
        settingData = settingData
    }

let coreSkillToMap (coreSkills: Skill Set) =
    coreSkills |> Set.map (fun x -> x.name, x) |> Map.ofSeq

open Skills
open VocationList
open Vocation

let update msg (model: Character) =

    let dicePoolCalculationData = characterToDicePoolCalculationData model

    match msg with
    | SetSettingData newSettingData -> {
        model with
            settingData = newSettingData
      }
    | SetName newName -> { model with name = newName }

    | AttributesMsg msg ->
        let newAttributes = Attributes.update msg model.attributes

        let newDicePoolCalculationData = {
            dicePoolCalculationData with
                attributes = newAttributes
        }

        let newCoreSkills =
            Skills.update (Skills.CalculateSkillDicePools newDicePoolCalculationData) model.coreSkills

        {
            model with
                attributes = newAttributes
                coreSkills = newCoreSkills
                vocationList =
                    model.vocationList
                    |> VocationList.update (VocationList.CalculateDicePools newDicePoolCalculationData)
                    |> VocationList.update (
                        VocationMsgForAll(
                            MundaneOrMagicVocationExtrasMsg(
                                MundaneOrMagicVocationExtras.RecalculateCoreSkillResourcePool(
                                    coreSkillToMap newCoreSkills
                                )
                            )
                        )
                    )
                combatSpeeds =
                    CombatSpeeds.update
                        (CombatSpeeds.RecalculateAllCombatSpeeds(newCoreSkills, newAttributes))
                        model.combatSpeeds
        }

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

    // Checking for InsertMundaneVocationSkill
    | VocationListMsg(VocationMsgAtPosition(position,
                                            Vocation.MundaneOrMagicVocationExtrasMsg(MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg(MundaneVocationSkills.InsertMundaneVocationSkill(name,
                                                                                                                                                                                            _,
                                                                                                                                                                                            Some weaponSkillDataMap))))) ->
        let newVocationList =
            VocationList.update
                (VocationMsgAtPosition(
                    position,
                    Vocation.MundaneOrMagicVocationExtrasMsg(
                        MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg(
                            MundaneVocationSkills.InsertMundaneVocationSkill(
                                name,
                                Some dicePoolCalculationData,
                                Some model.settingData.weaponSkillDataMap
                            )
                        )
                    )
                ))
                model.vocationList

        {
            model with
                vocationList = newVocationList
                combatRollList =
                    CombatRollList.update (
                        CombatRollList.RecalculateCombatRollList(
                            model.equipmentList,
                            vocationListToWeaponSkillList newVocationList,
                            model.settingData.weaponSkillDataMap,
                            dicePoolCalculationData
                        )
                    )
        }

    // Checking for InsertMagicVocationSkill
    | VocationListMsg(VocationMsgAtPosition(pos1,
                                            Vocation.MundaneOrMagicVocationExtrasMsg(MundaneOrMagicVocationExtras.MagicVocationExtrasMsg(MagicVocationExtras.MagicVocationSkillsMsg(MagicVocationSkills.InsertMagicVocationSkill(name,
                                                                                                                                                                                                                                 _,
                                                                                                                                                                                                                                 _,
                                                                                                                                                                                                                                 _,
                                                                                                                                                                                                                                 magicSkillDataMapOption)))))) ->
        let newVocationList =
            VocationList.update
                (VocationMsgAtPosition(
                    pos1,
                    Vocation.MundaneOrMagicVocationExtrasMsg(
                        MundaneOrMagicVocationExtras.MagicVocationExtrasMsg(
                            MagicVocationExtras.MagicVocationSkillsMsg(
                                MagicVocationSkills.InsertMagicVocationSkill(
                                    name,
                                    Some model.settingData.attributeNameSet,
                                    Some dicePoolCalculationData,
                                    Some model.settingData.weaponSkillDataMap,
                                    magicSkillDataMapOption
                                )
                            )
                        )
                    )
                ))
                model.vocationList

        {
            model with
                vocationList = newVocationList
                combatRollList =
                    CombatRollList.update (
                        CombatRollList.RecalculateCombatRollList(
                            model.equipmentList,
                            vocationListToWeaponSkillList newVocationList,
                            model.settingData.weaponSkillDataMap,
                            dicePoolCalculationData
                        )
                    )
        }

    // Checking for ModifySkillLevel in MundaneVocationSkill
    | VocationListMsg(VocationMsgAtPosition(pos1,
                                            Vocation.MundaneOrMagicVocationExtrasMsg(MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg(MundaneVocationSkills.ModifyMundaneVocationSkillAtPosition(pos2,
                                                                                                                                                                                                      MundaneVocationSkill.Msg.SkillMsg(Skill.Msg.ModifySkillLevel(msg,
                                                                                                                                                                                                                                                                   zeroToFiveOption,
                                                                                                                                                                                                                                                                   _))))))) ->
        let newVocationList =
            VocationList.update
                (VocationMsgAtPosition(
                    pos1,
                    Vocation.MundaneOrMagicVocationExtrasMsg(
                        MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg(
                            MundaneVocationSkills.ModifyMundaneVocationSkillAtPosition(
                                pos2,
                                MundaneVocationSkill.Msg.SkillMsg(
                                    Skill.Msg.ModifySkillLevel(msg, zeroToFiveOption, Some dicePoolCalculationData)
                                )
                            )
                        )
                    )
                ))
                model.vocationList

        {
            model with
                vocationList = newVocationList
                combatRollList =
                    CombatRollList.update (
                        CombatRollList.RecalculateCombatRollList(
                            model.equipmentList,
                            vocationListToWeaponSkillList newVocationList,
                            model.settingData.weaponSkillDataMap,
                            dicePoolCalculationData
                        )
                    )
        }

    // Checking for ZeroToFiveMsg
    | VocationListMsg(VocationMsgAtPosition(pos1, VocationStatMsg(VocationStat.ZeroToFiveMsg(msg, _)))) -> {
        model with
            vocationList =
                VocationList.update
                    (VocationMsgAtPosition(
                        pos1,
                        VocationStatMsg(VocationStat.ZeroToFiveMsg(msg, Some dicePoolCalculationData))
                    ))
                    model.vocationList
      }

    // Checking for ToggleGoverningAttribute for VocationStat
    | VocationListMsg(VocationMsgAtPosition(pos1, VocationStatMsg(VocationStat.ToggleGoveringAttribute(msg, _)))) -> {
        model with
            vocationList =
                VocationList.update
                    (VocationMsgAtPosition(
                        pos1,
                        VocationStatMsg(VocationStat.ToggleGoveringAttribute(msg, Some dicePoolCalculationData))
                    ))
                    model.vocationList
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
                            Some model.settingData.magicSystemMap
                        ))
                        model.vocationList
          }
        | _ -> {
            model with
                vocationList = VocationList.update msg model.vocationList
          }

    | EquipmentMsg(msg) ->
        let newEquipmentList = ItemStackList.update msg model.equipmentList

        {
            model with
                equipmentList = newEquipmentList
                combatRollList =
                    CombatRollList.update (
                        CombatRollList.RecalculateCombatRollList(
                            newEquipmentList,
                            vocationListToWeaponSkillList model.vocationList,
                            model.settingData.weaponSkillDataMap,
                            dicePoolCalculationData
                        )
                    )
        }

    | CharacterInformationMsg msg -> {
        model with
            characterInformation = CharacterInformation.update msg model.characterInformation
      }

    | EffectListMsg(msg) ->
        let newEffectList: FogentRoleplayLib.Effect.Effect list =
            EffectList.update msg model.characterEffects

        let newDicePoolCalculationData = {
            dicePoolCalculationData with
                effects = newEffectList
        }

        let newCoreSkills =
            Skills.update (Skills.CalculateSkillDicePools newDicePoolCalculationData) model.coreSkills

        {
            model with
                coreSkills = newCoreSkills
                characterEffects = newEffectList
                vocationList =
                    model.vocationList
                    |> VocationList.update (VocationList.CalculateDicePools newDicePoolCalculationData)
                    |> VocationList.update (
                        VocationMsgForAll(
                            MundaneOrMagicVocationExtrasMsg(
                                MundaneOrMagicVocationExtras.RecalculateCoreSkillResourcePool(
                                    coreSkillToMap newCoreSkills
                                )
                            )
                        )
                    )
                combatSpeeds =
                    CombatSpeeds.update
                        (CombatSpeeds.RecalculateAllCombatSpeeds(newCoreSkills, model.attributes))
                        model.combatSpeeds
                combatRollList =
                    CombatRollList.update (
                        CombatRollList.RecalculateCombatRollList(
                            model.equipmentList,
                            vocationListToWeaponSkillList model.vocationList,
                            model.settingData.weaponSkillDataMap,
                            dicePoolCalculationData
                        )
                    )
        }
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

        VocationList.view
            model.settingData.attributeNameSet
            (model.settingData.magicSystemMap.Keys |> Set.ofSeq)
            (model.settingData.weaponSkillDataMap.Keys)
            model.vocationList
            (VocationListMsg >> dispatch)

        CombatSpeeds.view
            (model.settingData.combatSpeedCalculationMap.Keys |> Set.ofSeq)
            model.combatSpeeds
            (CombatSpeedsMsg >> dispatch)

        // DestinyPoints.view model.destinyPoints (DestinyPointsMsg >> dispatch)

        EffectList.view (model.settingData.effectMap.Keys |> Set.ofSeq) model.characterEffects (fun msg ->
            (EffectListMsg msg |> dispatch))

        // CarryWeightStatOption.view
        //     carryWeightCalculationNameList
        //     model.carryWeightStatOption
        //     (CarryWeightStatOptionMsg >> dispatch)

        ItemStackList.view
            (model.settingData.itemStackMap.Keys |> Set.ofSeq)
            model.equipmentList
            ((fun msg -> EquipmentMsg msg) >> dispatch)

        CombatRollList.view model.combatRollList

        // ContainerList.view
        //     (List.collect itemToContainerClassNames (itemStackListToItemList allItemStackList))
        //     allItemStackNameList
        //     model.containerList
        //     (ContainerListMsg >> dispatch)

        CharacterInformation.view model.characterInformation (CharacterInformationMsg >> dispatch)
    ]