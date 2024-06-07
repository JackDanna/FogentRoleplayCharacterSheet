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
    | EquipmentMsg of ItemStackList.Msg * option<Map<string, WeaponSkillData>>
    | CharacterInformationMsg of CharacterInformation.Msg
    | EffectListMsg of EffectList.Msg * option<Map<string, WeaponSkillData>>
    | CombatSpeedsMsg of CombatSpeeds.Msg

let init (coreSkillDataSet: CoreSkillData Set) (settingData: SettingData) =
    let attributes = Set.map (fun x -> Attribute.init x.attributeName) coreSkillDataSet
    let effects = EffectList.init ()

    let dicePoolCalculationData: DicePoolCalculationData = {
        effects = effects
        attributes = attributes
    }

    {
        name = ""
        attributes = attributes
        coreSkills = Skills.initCoreSkills coreSkillDataSet dicePoolCalculationData
        vocationList = VocationList.init ()
        equipmentList = ItemStackList.init ()
        combatRollList = CombatRollList.init ()
        characterInformation = CharacterInformation.init ()
        characterEffects = effects
        combatSpeeds = CombatSpeeds.init ()
        settingData = FogentRoleplayLib.SettingData.init ()
    }

open Skills
open VocationList
open Vocation

let update msg (model: Character) =

    let coreSkillToMap (coreSkills: Skill Set) =
        coreSkills |> Set.map (fun x -> x.name, x) |> Map.ofSeq

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

    | VocationListMsg(VocationList.InsertVocation(x, y, z, _)) ->

        {
            model with
                vocationList =
                    VocationList.update
                        (VocationList.InsertVocation(x, y, z, Some model.settingData.magicSystemMap))
                        model.vocationList
        }

    // Check for InsertMundaneVocationSkill
    | VocationListMsg(VocationList.VocationMsgAtPosition(pos1,
                                                         Vocation.MundaneOrMagicVocationExtrasMsg(MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg(MundaneVocationSkills.InsertMundaneVocationSkill(x,
                                                                                                                                                                                                         y,
                                                                                                                                                                                                         _))))) ->

        {
            model with
                vocationList =
                    VocationList.update
                        (VocationList.VocationMsgAtPosition(
                            pos1,
                            Vocation.MundaneOrMagicVocationExtrasMsg(
                                MundaneOrMagicVocationExtras.MundaneVocationSkillsMsg(
                                    MundaneVocationSkills.InsertMundaneVocationSkill(
                                        x,
                                        y,
                                        Some model.settingData.weaponSkillDataMap
                                    )
                                )
                            )
                        ))
                        model.vocationList
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
                    CombatRollList.update
                        (CombatRollList.RecalculateCombatRollList(
                            model.equipmentList,
                            vocationListToWeaponSkillList newVocationList,
                            model.settingData.weaponSkillDataMap,
                            dicePoolCalculationData
                        ))
                        model.combatRollList
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
                    CombatRollList.update
                        (CombatRollList.RecalculateCombatRollList(
                            model.equipmentList,
                            vocationListToWeaponSkillList newVocationList,
                            model.settingData.weaponSkillDataMap,
                            dicePoolCalculationData
                        ))
                        model.combatRollList
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
                                Some weaponSkillDataMap
                            )
                        )
                    )
                ))
                model.vocationList

        {
            model with
                vocationList = newVocationList
                combatRollList =
                    CombatRollList.update
                        (CombatRollList.RecalculateCombatRollList(
                            model.equipmentList,
                            vocationListToWeaponSkillList newVocationList,
                            weaponSkillDataMap,
                            dicePoolCalculationData
                        ))
                        model.combatRollList
        }

    // Checking for InsertVocation
    | VocationListMsg(InsertVocation(x, _, _, y)) -> {
        model with
            vocationList =
                VocationList.update
                    (InsertVocation(x, Some(coreSkillToMap model.coreSkills), Some dicePoolCalculationData, y))
                    model.vocationList
      }

    | VocationListMsg(msg: VocationList.Msg) -> {
        model with
            vocationList = VocationList.update msg model.vocationList
      }

    | EquipmentMsg(msg, Some weaponSkillDataMap) ->
        let newEquipmentList = ItemStackList.update msg model.equipmentList

        {
            model with
                equipmentList = newEquipmentList
                combatRollList =
                    CombatRollList.update
                        (CombatRollList.RecalculateCombatRollList(
                            newEquipmentList,
                            vocationListToWeaponSkillList model.vocationList,
                            weaponSkillDataMap,
                            dicePoolCalculationData
                        ))
                        model.combatRollList
        }
    | CharacterInformationMsg msg -> {
        model with
            characterInformation = CharacterInformation.update msg model.characterInformation
      }
    | EffectListMsg(msg, Some weaponSkillDataMap) ->
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
                    CombatRollList.update
                        (CombatRollList.RecalculateCombatRollList(
                            model.equipmentList,
                            vocationListToWeaponSkillList model.vocationList,
                            weaponSkillDataMap,
                            dicePoolCalculationData
                        ))
                        model.combatRollList
        }
    | CombatSpeedsMsg msg ->
        match msg with
        | CombatSpeeds.Insert(name, _, _, combatSpeedMapOption) ->
            CombatSpeeds.Insert(name, Some(model.coreSkills), Some(model.attributes), combatSpeedMapOption)
        | _ -> msg
        |> (fun msg -> {
            model with
                combatSpeeds = CombatSpeeds.update msg model.combatSpeeds
        })

    | _ -> model

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
            (EffectListMsg(msg, None) |> dispatch))

        // CarryWeightStatOption.view
        //     carryWeightCalculationNameList
        //     model.carryWeightStatOption
        //     (CarryWeightStatOptionMsg >> dispatch)

        ItemStackList.view
            (model.settingData.itemStackMap.Keys |> Set.ofSeq)
            model.equipmentList
            ((fun msg -> EquipmentMsg(msg, None)) >> dispatch)

        CombatRollList.view model.combatRollList

        // ContainerList.view
        //     (List.collect itemToContainerClassNames (itemStackListToItemList allItemStackList))
        //     allItemStackNameList
        //     model.containerList
        //     (ContainerListMsg >> dispatch)

        CharacterInformation.view model.characterInformation (CharacterInformationMsg >> dispatch)
    ]