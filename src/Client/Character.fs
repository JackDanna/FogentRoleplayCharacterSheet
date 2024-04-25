module Character

open FogentRoleplayLib.Character
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.CoreSkill
open FogentRoleplayLib.ItemStack
open FogentRoleplayLib.Vocation

type Msg =
    | SetName of string
    | AttributeAndCoreSkillsListMsg of AttributeAndCoreSkillsList.Msg
    | VocationListMsg of VocationList.Msg
    | EquipmentMsg of ItemStackList.Msg
    | CharacterInformationMsg of CharacterInformation.Msg

let init (attributeNameSet: AttributeName Set) (coreSkillData: CoreSkill list) = {
    name = ""
    attributeAndCoreSkillsList = defaultAttributeAndCoreSkillsList attributeNameSet coreSkillData
    vocationList = VocationList.init ()
    equipmentList = []
    combatRollList = []
    characterInformation = CharacterInformation.init ()
}

let update msg (model: Character) =

    match msg with
    | SetName newName -> { model with name = newName }
    | AttributeAndCoreSkillsListMsg msg ->

        let newAttributeAndCoreSkillsList =
            AttributeAndCoreSkillsList.update msg model.attributeAndCoreSkillsList

        let dicePoolCalculationData =
            {
                model with
                    attributeAndCoreSkillsList = newAttributeAndCoreSkillsList
            }
            |> characterToDicePoolCalculation

        {
            model with
                attributeAndCoreSkillsList =
                    AttributeAndCoreSkillsList.update
                        (AttributeAndCoreSkillsList.Msg.CalculateDicePools(dicePoolCalculationData))
                        newAttributeAndCoreSkillsList
                vocationList =
                    VocationList.update (VocationList.CalculateDicePools(dicePoolCalculationData)) model.vocationList
        }
    | VocationListMsg(msg: VocationList.Msg) ->

        let newVocationList =
            VocationList.update msg model.vocationList
            |> VocationList.update (VocationList.CalculateDicePools(characterToDicePoolCalculation model))

        {
            model with
                vocationList = newVocationList
                combatRollList =
                    CombatRollList.update (
                        CombatRollList.RecalculateCombatRollList(
                            model.equipmentList,
                            vocationListToVocationSkillList newVocationList
                        )
                    )
        }
    | EquipmentMsg msg ->
        let newEquipmentList = ItemStackList.update msg model.equipmentList

        {
            model with
                equipmentList = newEquipmentList
                combatRollList =
                    CombatRollList.update (
                        CombatRollList.RecalculateCombatRollList(
                            newEquipmentList,
                            vocationListToVocationSkillList model.vocationList
                        )
                    )
        }
    | CharacterInformationMsg msg -> {
        model with
            characterInformation = CharacterInformation.update msg model.characterInformation
      }

open Feliz
open Feliz.Bulma

let view
    attributeNameSet
    (allItemStackList: Map<string, ItemStack>)
    (vocationSkillData: VocationSkillData)
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

        VocationList.view attributeNameSet vocationSkillData model.vocationList (VocationListMsg >> dispatch)

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

        ItemStackList.view allItemStackList model.equipmentList (EquipmentMsg >> dispatch)

        CombatRollList.view model.combatRollList
        //(CombatRollListMsg >> dispatch)

        // ContainerList.view
        //     (List.collect itemToContainerClassNames (itemStackListToItemList allItemStackList))
        //     allItemStackNameList
        //     model.containerList
        //     (ContainerListMsg >> dispatch)

        CharacterInformation.view model.characterInformation (CharacterInformationMsg >> dispatch)
    ]