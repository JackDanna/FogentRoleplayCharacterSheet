module Character

open FogentRoleplayLib.Character
open FogentRoleplayLib.Attribute
open FogentRoleplayLib.CoreSkill
open FogentRoleplayLib.Skill
open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.AttributeAndCoreSkills

type Msg =
    | SetName of string
    | AttributeAndCoreSkillsListMsg of AttributeAndCoreSkillsList.Msg

let init (attributeData: Attribute list) (coreSkillData: CoreSkill list) = {
    name = ""
    attributeAndCoreSkillsList = defaultAttributeAndCoreSkillsList attributeData coreSkillData
}

let update msg (model: Character) =
    match msg with
    | SetName newName -> { model with name = newName }
    | AttributeAndCoreSkillsListMsg msg ->

        let calculationData: DicePoolCalculationData = {
            baseDice = None
            AttributeStatList =
                List.map
                    (fun attributeAndCoreSkills -> attributeAndCoreSkills.attributeStat)
                    model.attributeAndCoreSkillsList
            injuryDicePenalty = 0u
            weightClassDicePenalty = 0u
            itemEffectDicePoolMod = createD6DicePoolMod 0u
        }

        {
            model with
                attributeAndCoreSkillsList =
                    model.attributeAndCoreSkillsList
                    |> AttributeAndCoreSkillsList.update msg
                    |> AttributeAndCoreSkillsList.update (
                        AttributeAndCoreSkillsList.Msg.CalculateDicePools(calculationData)
                    )
        }

open Feliz
open Feliz.Bulma

let view (model: Character) dispatch =
    // let allItemStackNameList =
    //     (List.map (fun (itemStack: ItemStack) -> itemStack.item.name) allItemStackList)

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

    // VocationList.view
    //     combatVocationalSkill
    //     (vocationDicePoolListToStringifiedVocationDicePoolList model.vocationDicePoolList)
    //     (attributesToAttributeNames model.attributeList)
    //     model.vocationList
    //     (VocationListMsg >> dispatch)

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