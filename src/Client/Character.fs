module Character

open FogentRoleplayLib.Character
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.CoreSkill
open FogentRoleplayLib.Skill
open FogentRoleplayLib.DicePoolMod
open FogentRoleplayLib.AttributeAndCoreSkills

type Msg =
    | SetName of string
    | AttributeAndCoreSkillsListMsg of AttributeAndCoreSkillsList.Msg

let init (attributeNameSet: AttributeName Set) (coreSkillData: CoreSkill list) = {
    name = ""
    attributeAndCoreSkillsList = defaultAttributeAndCoreSkillsList attributeNameSet coreSkillData
    vocationList = VocationList.init ()
}

let update msg (model: Character) =

    match msg with
    | SetName newName -> { model with name = newName }
    | AttributeAndCoreSkillsListMsg msg ->

        let newAttributeAndCoreSkillsList =
            AttributeAndCoreSkillsList.update msg model.attributeAndCoreSkillsList

        {
            model with
                attributeAndCoreSkillsList =
                    AttributeAndCoreSkillsList.update
                        (AttributeAndCoreSkillsList.Msg.CalculateDicePools(
                            {
                                baseDice = None
                                AttributeList =
                                    List.map
                                        (fun attributeAndCoreSkills -> attributeAndCoreSkills.attributeStat)
                                        newAttributeAndCoreSkillsList
                                injuryDicePenalty = 0u
                                weightClassDicePenalty = 0u
                                itemEffectDicePoolMod = createD6DicePoolMod 0u
                            }
                        ))
                        newAttributeAndCoreSkillsList
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