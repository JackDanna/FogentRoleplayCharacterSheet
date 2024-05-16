namespace Shared

open FogentRoleplayLib.AttributeName
open FogentRoleplayLib.AttributeAndCoreSkillsData
open FogentRoleplayLib.ItemStack
open FogentRoleplayLib.WeaponSpell
open FogentRoleplayLib.MagicSystem
open FogentRoleplayLib.WeaponSkillData

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type FogentRoleplayData = {
    attributeNameSet: AttributeName Set
    attributeAndCoreSkillDataSet: AttributeAndCoreSkillsData Set
    itemStackMap: Map<string, ItemStack>
    weaponSpellSet: WeaponSpell Set
    magicSystemMap: Map<string, MagicSystem>
    weaponSkillData: WeaponSkillData Set
//   effectForDisplayMap: Map<string, EffectForDisplay>
//   carryWeightCalculationMap: Map<string, CarryWeightCalculation>
//   weightClassList: WeightClass List
//   movementSpeedCalculationMap: Map<string, MovementSpeedCalculation>
}

type IFogentRoleplayDataApi = {
    getInitData: unit -> Async<FogentRoleplayData>
}