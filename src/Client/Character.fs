module Character

open FogentRoleplayLib.Character
open FogentRoleplayLib.WeaponSkillData
open FogentRoleplayLib.SkillName
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.CoreSkillData
open FogentRoleplayLib.DicePoolCalculation
open FogentRoleplayLib.Skill

type Msg =
    | SetName of string
    | AttributesMsg of Attributes.Msg
    | CoreSkillsMsg of Skills.Msg
    | VocationListMsg of VocationList.Msg
    | EquipmentMsg of ItemStackList.Msg * option<Set<WeaponSkillData>>
    | CharacterInformationMsg of CharacterInformation.Msg
    | EffectListMsg of EffectList.Msg
    | CombatSpeedsMsg of CombatSpeeds.Msg

let init (coreSkillDataSet: CoreSkillData Set) =
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
    }

open VocationList
open Vocation
open MundaneVocation
open MundaneVocationSkills

let update msg (model: Character) =

    let coreSkillToMap (coreSkills: Skill Set) =
        coreSkills |> Set.map (fun x -> x.name, x) |> Map.ofSeq

    let dicePoolCalculationData = characterToDicePoolCalculationData model

    match msg with
    | SetName newName -> { model with name = newName }

    | AttributesMsg msg ->
        let newAttributes = Attributes.update msg model.attributes

        let newDicePoolCalculationData = {
            dicePoolCalculationData with
                attributes = newAttributes
        }

        {
            model with
                attributes = newAttributes
                coreSkills = Skills.update (Skills.CalculateSkillDicePools newDicePoolCalculationData) model.coreSkills
        }
    // | ModifyAttributeAndCoreSkillsList(pos1, tempMsg) ->
    //     match tempMsg with
    //     | CoreSkillListMsg(ModifiedCoreSkillAtPosition(pos2, SkillMsg(ModifySkillLevel(x, y, z, _)))) -> {
    //         model with
    //             attributeAndCoreSkillsList =
    //                 AttributeAndCoreSkillsList.update
    //                     (ModifyAttributeAndCoreSkillsList(
    //                         pos1,
    //                         CoreSkillListMsg(
    //                             ModifiedCoreSkillAtPosition(
    //                                 pos2,
    //                                 SkillMsg(ModifySkillLevel(x, y, z, Some dicePoolCalculationData))
    //                             )
    //                         )
    //                     ))
    //                     model.attributeAndCoreSkillsList
    //       }

    //     | AttributeMsg(msg, _) ->
    //         let newAttributeAndCoreSkillsList =
    //             AttributeAndCoreSkillsList.update
    //                 (ModifyAttributeAndCoreSkillsList(pos1, AttributeMsg(msg, Some dicePoolCalculationData.effects)))
    //                 model.attributeAndCoreSkillsList

    //         {
    //             model with
    //                 attributeAndCoreSkillsList = newAttributeAndCoreSkillsList
    //                 vocationList =
    //                     VocationList.update
    //                         (VocationList.CalculateDicePools {
    //                             dicePoolCalculationData with
    //                                 attributes = attributeAndCoreSkillsSetToAttributes newAttributeAndCoreSkillsList
    //                         })
    //                         model.vocationList
    //         }
    //     | _ -> model

    // | _ -> {
    //     model with
    //         attributeAndCoreSkillsList = AttributeAndCoreSkillsList.update msg model.attributeAndCoreSkillsList
    //   }

    | CoreSkillsMsg msg -> {
        model with
            coreSkills = Skills.update msg model.coreSkills
      }

    | VocationListMsg(msg: VocationList.Msg) ->
        let temp msg =
            match msg with

            | VocationMsgAtPosition(position, msg) ->
                match msg with
                | MundaneVocationMsg(MundaneVocationSkillsMsg(InsertSkill(skillName, weaponSkillDataMapOption, _))) ->

                    MundaneVocationMsg(
                        MundaneVocationSkillsMsg(
                            InsertSkill(skillName, weaponSkillDataMapOption, Some dicePoolCalculationData)
                        )
                    )
                | _ -> msg
                |> (fun msg -> VocationMsgAtPosition(position, msg))
            | InsertVocation(x, _, _, y) ->

                (InsertVocation(x, Some(coreSkillToMap model.coreSkills), Some dicePoolCalculationData, y))
            | _ -> msg


        {
            model with
                vocationList = VocationList.update (temp msg) model.vocationList
        }

    // {
    //     model with
    //         vocationList = newVocationList
    //         combatRollList =
    //             CombatRollList.update (
    //                 CombatRollList.RecalculateCombatRollList(
    //                     model.equipmentList,
    //                     vocationListToWeaponSkillList newVocationList
    //                 )
    //             )
    // }
    | EquipmentMsg(msg, Some weaponSkillData) ->
        let newEquipmentList = ItemStackList.update msg model.equipmentList

        {
            model with
                equipmentList = newEquipmentList
                combatRollList =
                    CombatRollList.update
                        (CombatRollList.RecalculateCombatRollList(
                            newEquipmentList,
                            vocationListToWeaponSkillList model.vocationList,
                            weaponSkillData,
                            dicePoolCalculationData
                        ))
                        model.combatRollList
        }
    | CharacterInformationMsg msg -> {
        model with
            characterInformation = CharacterInformation.update msg model.characterInformation
      }
    | EffectListMsg msg ->
        let newEffectList: FogentRoleplayLib.Effect.Effect list =
            EffectList.update msg model.characterEffects

        {
            model with
                characterEffects = newEffectList
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

let view
    attributeNameSet
    (allItemStackNameSet: string Set)
    (magicSystemNameSet: string Set)
    (weaponSkillNameSet)
    (effectNameSet: string Set)
    combatSpeedsCalculationNames
    (model: Character)
    dispatch
    =

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
            attributeNameSet
            magicSystemNameSet
            weaponSkillNameSet
            model.vocationList
            (VocationListMsg >> dispatch)

        CombatSpeeds.view combatSpeedsCalculationNames model.combatSpeeds (CombatSpeedsMsg >> dispatch)

        // DestinyPoints.view model.destinyPoints (DestinyPointsMsg >> dispatch)

        EffectList.view effectNameSet model.characterEffects (EffectListMsg >> dispatch)

        // CarryWeightStatOption.view
        //     carryWeightCalculationNameList
        //     model.carryWeightStatOption
        //     (CarryWeightStatOptionMsg >> dispatch)

        ItemStackList.view allItemStackNameSet model.equipmentList ((fun msg -> EquipmentMsg(msg, None)) >> dispatch)

        CombatRollList.view model.combatRollList

        // ContainerList.view
        //     (List.collect itemToContainerClassNames (itemStackListToItemList allItemStackList))
        //     allItemStackNameList
        //     model.containerList
        //     (ContainerListMsg >> dispatch)

        CharacterInformation.view model.characterInformation (CharacterInformationMsg >> dispatch)
    ]