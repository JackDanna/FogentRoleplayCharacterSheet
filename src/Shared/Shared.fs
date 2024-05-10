namespace Shared

// open FogentRoleplayLib.Item
// open FogentRoleplayLib.MagicSkill
// open FogentRoleplayLib.MagicCombat
// open FogentRoleplayLib.Range
// open FogentRoleplayLib.EffectForDisplay
// open FogentRoleplayLib.CarryWeightCalculation
// open FogentRoleplayLib.MovementSpeedEffect
// open FogentRoleplayLib.WeightClass
open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.AttributeAndCoreSkillsData
open FogentRoleplayLib.ItemStack
open FogentRoleplayLib.Skill
open FogentRoleplayLib.WeaponSpell
open FogentRoleplayLib.MagicSkill
open FogentRoleplayLib.Character
open FogentRoleplayLib.MagicSystem
// open FogentRoleplayLib.ItemStack

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type FogentRoleplayData = {
    attributeNameSet: AttributeName Set
    coreSkillDataSet: AttributeAndCoreSkillsData Set
    itemStackMap: Map<string, ItemStack>
    weaponSpellSet: WeaponSpell Set
    vocationSkillData: VocationSkillData
//   magicSkillMap: Map<string, MagicSkill>
//   magicCombatMap: Map<string, MagicCombat>
//   rangeMap: Map<string, Range>
//   combatVocationalSkill: string list
//   effectForDisplayMap: Map<string, EffectForDisplay>
//   carryWeightCalculationMap: Map<string, CarryWeightCalculation>
//   weightClassList: WeightClass List
//   movementSpeedCalculationMap: Map<string, MovementSpeedCalculation>
}

type IFogentRoleplayDataApi = {
    getInitData: unit -> Async<FogentRoleplayData>
}