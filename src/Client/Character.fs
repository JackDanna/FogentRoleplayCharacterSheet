module Character

open FogentRoleplayLib.Character
open FogentRoleplayLib.ItemStack
open FogentRoleplayLib.Effect
open FogentRoleplayLib.WeaponSkillData
open FogentRoleplayLib.AttributeAndCoreSkillsData

type Msg =
    | SetName of string
    | AttributeAndCoreSkillsListMsg of AttributeAndCoreSkillsList.Msg
    | VocationListMsg of VocationList.Msg
    | EquipmentMsg of ItemStackList.Msg * option<Set<WeaponSkillData>>
    | CharacterInformationMsg of CharacterInformation.Msg

let init (attributeAndCoreSkillDataList: AttributeAndCoreSkillsData Set) =
    let effects: Effect List = []

    {
        name = ""
        attributeAndCoreSkillsList = AttributeAndCoreSkillsList.init effects attributeAndCoreSkillDataList
        vocationList = VocationList.init ()
        equipmentList = ItemStackList.init ()
        combatRollList = CombatRollList.init ()
        characterInformation = CharacterInformation.init ()
        characterEffects = effects
    }

open VocationList
open Vocation
open MundaneVocation
open MundaneVocationSkills

open AttributeAndCoreSkillsList
open AttributeAndCoreSkills
open CoreSkills
open CoreSkill
open Skill

open FogentRoleplayLib.AttributeAndCoreSkills

let update msg (model: Character) =
    let dicePoolCalculationData = characterToDicePoolCalculationData model

    match msg with
    | SetName newName -> { model with name = newName }

    | AttributeAndCoreSkillsListMsg msg ->

        match msg with
        | ModifyAttributeAndCoreSkillsList(pos1, tempMsg) ->
            match tempMsg with
            | CoreSkillListMsg(ModifiedCoreSkillAtPosition(pos2, SkillMsg(ModifySkillLevel(x, y, z, _)))) -> {
                model with
                    attributeAndCoreSkillsList =
                        AttributeAndCoreSkillsList.update
                            (ModifyAttributeAndCoreSkillsList(
                                pos1,
                                CoreSkillListMsg(
                                    ModifiedCoreSkillAtPosition(
                                        pos2,
                                        SkillMsg(ModifySkillLevel(x, y, z, Some dicePoolCalculationData))
                                    )
                                )
                            ))
                            model.attributeAndCoreSkillsList
              }

            | AttributeMsg(msg, _) ->
                let newAttributeAndCoreSkillsList =
                    AttributeAndCoreSkillsList.update
                        (ModifyAttributeAndCoreSkillsList(pos1, AttributeMsg(msg, Some dicePoolCalculationData.effects)))
                        model.attributeAndCoreSkillsList

                {
                    model with
                        attributeAndCoreSkillsList = newAttributeAndCoreSkillsList
                        vocationList =
                            VocationList.update
                                (VocationList.CalculateDicePools {
                                    dicePoolCalculationData with
                                        attributes = attributeAndCoreSkillsSetToAttributes newAttributeAndCoreSkillsList
                                })
                                model.vocationList
                }
            | _ -> model

        | _ -> {
            model with
                attributeAndCoreSkillsList = AttributeAndCoreSkillsList.update msg model.attributeAndCoreSkillsList
          }

    // | VocationListMsg(VocationMsgAtPosition(pos, MundaneVocationMsg(MundaneVocationSkillsMsg(WeaponSkillListMsg(WeaponSkillList))))) ->
    //     match munMsg with
    //                 | MundaneVocationSkillsMsg(VocationalSkillListMsg())
    // | VocationListMsg(VocationMsgAtPosition(pos, MundaneVocationMsg(MundaneVocationSkillsMsg(WeaponSkillListMsg(WeaponSkillList))))) ->

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
                (InsertVocation(
                    x,
                    Some(attributeAndCoreSkillsSetToSkillMap model.attributeAndCoreSkillsList),
                    Some dicePoolCalculationData,
                    y
                ))
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
    | _ -> model

open Feliz
open Feliz.Bulma

let view
    attributeNameSet
    (allItemStackList: Map<string, ItemStack>)
    (magicSystemNameSet: string Set)
    (weaponSkillNameSet)
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

        AttributeAndCoreSkillsList.view model.attributeAndCoreSkillsList (AttributeAndCoreSkillsListMsg >> dispatch)

        VocationList.view
            attributeNameSet
            magicSystemNameSet
            weaponSkillNameSet
            model.vocationList
            (VocationListMsg >> dispatch)

        // DestinyPoints.view model.destinyPoints (DestinyPointsMsg >> dispatch)

        // CharacterEffectForDisplayList.view
        //     characterEffectKeyList
        //     model.characterEffectForDisplayList
        //     (CharacterEffectListMsg >> dispatch)

        // CarryWeightStatOption.view
        //     carryWeightCalculationNameList
        //     model.carryWeightStatOption
        //     (CarryWeightStatOptionMsg >> dispatch)

        // EquipmentEffectForDisplayList.view model.equipmentEffectForDisplayList

        ItemStackList.view allItemStackList model.equipmentList ((fun msg -> EquipmentMsg(msg, None)) >> dispatch)

        CombatRollList.view model.combatRollList

        // ContainerList.view
        //     (List.collect itemToContainerClassNames (itemStackListToItemList allItemStackList))
        //     allItemStackNameList
        //     model.containerList
        //     (ContainerListMsg >> dispatch)

        CharacterInformation.view model.characterInformation (CharacterInformationMsg >> dispatch)
    ]