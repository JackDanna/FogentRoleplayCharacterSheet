module Character

open FogentRoleplayLib.Character
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.CoreSkill

type Msg =
    | SetName of string
    | AttributeAndCoreSkillsListMsg of AttributeAndCoreSkillsList.Msg
    | VocationListMsg of VocationList.Msg

let init (attributeNameSet: AttributeName Set) (coreSkillData: CoreSkill list) = {
    name = ""
    attributeAndCoreSkillsList = defaultAttributeAndCoreSkillsList attributeNameSet coreSkillData
    vocationList = VocationList.init ()
    equipmentList = []
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

        let newVocationList = VocationList.update msg model.vocationList

        {
            model with
                vocationList =
                    VocationList.update
                        (VocationList.CalculateDicePools(characterToDicePoolCalculation model))
                        newVocationList
        }

open Feliz
open Feliz.Bulma

let view attributeNameSet (model: Character) dispatch =

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

        VocationList.view attributeNameSet model.vocationList (VocationListMsg >> dispatch)

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

    // EquipmentList.view allItemStackNameList model.equipmentList (EquipmentListMsg >> dispatch)

    // CombatRollTable.view model.combatRollList

    // ContainerList.view
    //     (List.collect itemToContainerClassNames (itemStackListToItemList allItemStackList))
    //     allItemStackNameList
    //     model.containerList
    //     (ContainerListMsg >> dispatch)

    // CharacterInformation.view model.characterInformation (CharacterInformationMsg >> dispatch)
    ]