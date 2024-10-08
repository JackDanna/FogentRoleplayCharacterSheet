module DatabaseUtils

let damagageTypeTableName = "DamageTypeData"
let engageableOpponentsCalculationTableName = "EngageableOpponentsCalculationData"
let calculatedRangeTableName = "CalculatedRangeData"
let rangeCalculationTableName = "RangeCalculationData"
let sphereCalculationTableName = "SphereCalculation"
let coneCalculationTableName = "ConeCalculation"
let setSphereTableName = "SetSphere"
let setConeTableName = "SetCone"
let resourceNameTableName = "ResourceClassData"
let coreSkillDataTableName = "CoreSkillData"
let magicSkillTableName = "MagicSkillData"
let magicSystemTableName = "MagicSystemData"
let weaponClassTableName = "WeaponClassData"
let weaponSkillTableName = "WeaponSkillData"
let weaponSpellTableName = "WeaponSpellData"
let containerTableName = "ContainerClassData"
let weaponResourceTableName = "WeaponResourceClassData"
let physicalDefenseTableName = "PhysicalDefenseEffect"
let skillDiceModTableName = "SkillDiceModEffect"
let attributeStatAdjustmentTableName = "AttributeStatAdjustment"
let attributeDeterminedDiceModTableName = "AttributeDeterminedDiceMod"
let baseDiceTierTableName = "BaseDiceTierData"
let weightClassTableName = "WeightClassData"
let speedCalculationTableName = "Speed"
let combatSpeedCalculationTableName = "CombatSpeed"
let carryWeightCalculationTableName = "CarryWeightCalculationData"
let textEffectTableName = "TextEffect"
let itemElementTableName = "ItemData"

let tableNameToCSVFileName tableName = $"{tableName}.csv"