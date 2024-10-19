module LiteDBDatabase

open LiteDB
open LiteDB.FSharp
open Shared

open FogentRoleplayLib.Character
open FogentRoleplayLib.Setting

// let insertEntity (entity: 'T) =
//     // Since Id is set to 0, inserted entities will be placed according to auto-incrementation
//     let idEntity = { Id = 0; Entity = entity }
//     collectionFromDB.Insert(idEntity) |> ignore
//     idEntity

// let findEntity entity = collectionFromDB.FindById entity.Id

// let insertEntities entities =
//     entities |> Seq.map (fun entity -> insertEntity entity)

//users.EnsureIndex(fun (u: IdUser) -> u.Entity.userName) |> ignore



// [<CLIMutable>]
// type UserCharacterAccess = {
//     Id: int
//     UserId: int
//     SettingId: int
//     CharacterId: int
// //AccessGranted: System.DateTime
// //AccessType: string
// }

[<AutoOpenAttribute>]
module LiteDBTypes =
    open FogentRoleplayLib
    open Attribute
    open Skill
    open ZeroToThree
    open Vocation
    open DicePoolCalculation
    open ItemElement
    open CombatRoll
    open CharacterInformation
    open Effect
    open CombatSpeed
    open SettingData
    open WeightClass
    open CarryWeightCalculation
    open CoreSkillData
    open ListUtils
    open WeightClass
    open SkillName
    open Neg1To5
    open AttributeName
    open DicePoolMod
    open ZeroToFive
    open DicePool
    open MundaneOrMagicVocationExtras
    open DamageType
    open Range
    open AreaOfEffect
    open ResourceName
    open Penetration
    open EngageableOpponents
    open DurationAndSource
    open CalculatedRange
    open SetAreaOfEffect
    open WeaponSpell
    open CombatSpeedCalculation
    open WeaponResource
    open Weapon
    open AttributeDeterminedDiceMod
    open FogentRoleplayLib.Item
    open FogentRoleplayLib.WeaponSkillData
    open FogentRoleplayLib.VocationStat
    open FogentRoleplayLib.MundaneVocationSkill
    open FogentRoleplayLib.MagicSkillData
    open FogentRoleplayLib.MagicSystem
    open FogentRoleplayLib.MagicVocationExtras
    open FogentRoleplayLib.SkillDiceMod
    open FogentRoleplayLib.AttributeStatAdjustment
    open FogentRoleplayLib.PhysicalDefense
    open FogentRoleplayLib.BaseDiceMod
    open FogentRoleplayLib.TextEffect
    open FogentRoleplayLib.ItemStack
    open FogentRoleplayLib.Container
    open FogentRoleplayLib.BaseDiceTier
    open FogentRoleplayLib.Setting

    type LiteDB_WeaponResource = {
        name: string
        resourceName: ResourceName
        dicePoolMod: DicePoolMod
        penetration: Penetration
        rangeOption: Range option
        damageTypeSet: DamageType seq
        namedAreaOfEffectOption: AreaOfEffect option
    }

    let toWeaponResource x : WeaponResource = {
        name = x.name
        resourceName = x.resourceName
        dicePoolMod = x.dicePoolMod
        penetration = x.penetration
        rangeOption = x.rangeOption
        damageTypeSet = Set.ofSeq x.damageTypeSet
        namedAreaOfEffectOption = x.namedAreaOfEffectOption
    }

    let toLiteDB_WeaponResource (x: WeaponResource) = {
        name = x.name
        resourceName = x.resourceName
        dicePoolMod = x.dicePoolMod
        penetration = x.penetration
        rangeOption = x.rangeOption
        damageTypeSet = x.damageTypeSet
        namedAreaOfEffectOption = x.namedAreaOfEffectOption
    }

    type LiteDB_Weapon = {
        name: string
        governingSkillName: SkillName
        oneHandedDiceMod: DicePoolMod option
        twoHandedDiceMod: DicePoolMod option
        penetration: Penetration
        range: Range
        damageTypes: DamageType seq
        engageableOpponents: EngageableOpponents
        dualWieldedDiceMod: DicePoolMod option
        areaOfEffectOption: AreaOfEffect option
        resourceNameOption: ResourceName option
        baseDiceTier: BaseDiceTier
    }

    let toWeapon (x: LiteDB_Weapon) : Weapon = {
        name = x.name
        governingSkillName = x.governingSkillName
        oneHandedDiceMod = x.oneHandedDiceMod
        twoHandedDiceMod = x.twoHandedDiceMod
        penetration = x.penetration
        range = x.range
        damageTypes = Set.ofSeq x.damageTypes
        engageableOpponents = x.engageableOpponents
        dualWieldedDiceMod = x.dualWieldedDiceMod
        areaOfEffectOption = x.areaOfEffectOption
        resourceNameOption = x.resourceNameOption
        baseDiceTier = x.baseDiceTier
    }

    let toLiteDB_Weapon (x: Weapon) : LiteDB_Weapon = {
        name = x.name
        governingSkillName = x.governingSkillName
        oneHandedDiceMod = x.oneHandedDiceMod
        twoHandedDiceMod = x.twoHandedDiceMod
        penetration = x.penetration
        range = x.range
        damageTypes = x.damageTypes
        engageableOpponents = x.engageableOpponents
        dualWieldedDiceMod = x.dualWieldedDiceMod
        areaOfEffectOption = x.areaOfEffectOption
        resourceNameOption = x.resourceNameOption
        baseDiceTier = x.baseDiceTier
    }

    type LiteDB_AttributeDeterminedDiceMod = {
        name: string
        attributesToEffect: AttributeName seq
        dicePoolMod: DicePoolMod
        durationAndSource: DurationAndSource
    }

    let toAttributeDeterminedDiceMod x : AttributeDeterminedDiceMod = {
        name = x.name
        attributesToEffect = Set.ofSeq x.attributesToEffect
        dicePoolMod = x.dicePoolMod
        durationAndSource = x.durationAndSource
    }

    let toLiteDB_AttributeDeterminedDiceMod (x: AttributeDeterminedDiceMod) : LiteDB_AttributeDeterminedDiceMod = {
        name = x.name
        attributesToEffect = x.attributesToEffect
        dicePoolMod = x.dicePoolMod
        durationAndSource = x.durationAndSource
    }

    type LiteDB_Effect =
        | Weapon of LiteDB_Weapon
        | WeaponResource of LiteDB_WeaponResource
        | SkillDiceMod of SkillDiceMod
        | AttributeStatAdjustment of AttributeStatAdjustment
        | PhysicalDefense of PhysicalDefense
        | AttributeDeterminedDiceMod of LiteDB_AttributeDeterminedDiceMod
        | BaseDiceMod of BaseDiceMod
        | TextEffect of TextEffect

    let toEffect =
        function
        | Weapon weapon -> toWeapon weapon |> Effect.Weapon
        | WeaponResource weaponResource -> toWeaponResource weaponResource |> Effect.WeaponResource
        | SkillDiceMod x -> Effect.SkillDiceMod x
        | AttributeStatAdjustment x -> Effect.AttributeStatAdjustment x
        | PhysicalDefense x -> Effect.PhysicalDefense x
        | AttributeDeterminedDiceMod x -> toAttributeDeterminedDiceMod x |> Effect.AttributeDeterminedDiceMod
        | BaseDiceMod x -> Effect.BaseDiceMod x
        | TextEffect x -> Effect.TextEffect x

    let toLiteDB_Effect =
        function
        | Effect.Weapon weapon -> toLiteDB_Weapon weapon |> Weapon
        | Effect.WeaponResource weaponResource -> toLiteDB_WeaponResource weaponResource |> WeaponResource
        | Effect.SkillDiceMod x -> SkillDiceMod x
        | Effect.AttributeStatAdjustment x -> AttributeStatAdjustment x
        | Effect.PhysicalDefense x -> PhysicalDefense x
        | Effect.AttributeDeterminedDiceMod x -> toLiteDB_AttributeDeterminedDiceMod x |> AttributeDeterminedDiceMod
        | Effect.BaseDiceMod x -> BaseDiceMod x
        | Effect.TextEffect x -> TextEffect x

    type LiteDB_Item = {
        name: string
        itemEffectSet: LiteDB_Effect seq
        value: string
        weight: float
    }

    let toItem x : Item = {
        name = x.name
        itemEffectSet = x.itemEffectSet |> Seq.map toEffect |> Set.ofSeq
        value = x.value
        weight = x.weight
    }

    let toLiteDB_Item (x: Item) = {
        name = x.name
        itemEffectSet = x.itemEffectSet |> Seq.map toLiteDB_Effect
        value = x.value
        weight = x.weight
    }

    type LiteDB_ItemStack = { item: LiteDB_Item; quantity: uint }

    let toItemStack x : ItemStack = {
        item = toItem x.item
        quantity = x.quantity
    }

    let toLiteDB_ItemStack (x: ItemStack) = {
        item = toLiteDB_Item x.item
        quantity = x.quantity
    }

    type LiteDB_ContainerItem = {
        item: LiteDB_Item
        containerTypeData: Container
        containedElements: LiteDB_ItemElement list
    }

    and LiteDB_ItemElement =
        | Item of LiteDB_Item
        | ContainerItem of LiteDB_ContainerItem
        | ItemStack of LiteDB_ItemStack

    let rec toContainerItem x : ContainerItem = {
        item = toItem x.item
        containerTypeData = x.containerTypeData
        containedElements = x.containedElements |> List.map toItemElement
    }

    and toItemElement =
        function
        | Item item -> toItem item |> ItemElement.Item
        | ContainerItem x -> toContainerItem x |> ItemElement.ContainerItem
        | ItemStack x -> toItemStack x |> ItemElement.ItemStack

    let rec toLiteDB_ContainerItem (x: ContainerItem) = {
        item = toLiteDB_Item x.item
        containerTypeData = x.containerTypeData
        containedElements = x.containedElements |> List.map toLiteDB_ItemElement
    }

    and toLiteDB_ItemElement =
        function
        | ItemElement.Item item -> toLiteDB_Item item |> Item
        | ItemElement.ContainerItem x -> toLiteDB_ContainerItem x |> ContainerItem
        | ItemElement.ItemStack x -> toLiteDB_ItemStack x |> ItemStack

    type LiteDB_Skill = {
        name: SkillName
        level: Neg1To5
        governingAttributeNames: AttributeName seq
        dicePool: DicePool
    }

    let toSkill x : Skill = {
        name = x.name
        level = x.level
        governingAttributeNames = Set.ofSeq x.governingAttributeNames
        dicePool = x.dicePool
    }

    let toLiteDB_Skill (x: Skill) = {
        name = x.name
        level = x.level
        governingAttributeNames = x.governingAttributeNames
        dicePool = x.dicePool
    }

    type LiteDB_WeaponSkillData = {
        name: string
        governingAttributes: AttributeName seq
    }

    let toWeaponSkillData x : WeaponSkillData = {
        name = x.name
        governingAttributes = Set.ofSeq x.governingAttributes
    }

    let toLiteDB_WeaponSkillData (x: WeaponSkillData) = {
        name = x.name
        governingAttributes = x.governingAttributes
    }

    type LiteDB_VocationStat = {
        name: string
        governingAttributeNameSet: AttributeName seq
        level: ZeroToFive
        dicePool: DicePool
    }

    let toVocationStat x : VocationStat = {
        name = x.name
        governingAttributeNameSet = Set.ofSeq x.governingAttributeNameSet
        level = x.level
        dicePool = x.dicePool
    }

    let toLiteDB_VocationStat (x: VocationStat) = {
        name = x.name
        governingAttributeNameSet = x.governingAttributeNameSet
        level = x.level
        dicePool = x.dicePool
    }

    type LiteDB_MundaneVocationSkill =
        | VocationalSkill of LiteDB_Skill
        | WeaponSkill of LiteDB_Skill

    let toMundaneVocationSkill =
        function
        | VocationalSkill skill -> toSkill skill |> MundaneVocationSkill.VocationalSkill
        | WeaponSkill skill -> toSkill skill |> MundaneVocationSkill.WeaponSkill

    let toLiteDB_MundaneVocationSkill =
        function
        | MundaneVocationSkill.VocationalSkill skill -> toLiteDB_Skill skill |> VocationalSkill
        | MundaneVocationSkill.WeaponSkill skill -> toLiteDB_Skill skill |> WeaponSkill

    type LiteDB_MagicSkillData = {
        name: SkillName
        damageTypes: DamageType seq
        isMeleeCapable: bool
        isRangeCapable: bool
    }

    let toMagicSkillData x : MagicSkillData = {
        name = x.name
        damageTypes = x.damageTypes |> Set.ofSeq
        isMeleeCapable = x.isMeleeCapable
        isRangeCapable = x.isRangeCapable
    }

    let toLiteDB_MagicSkillData (x: MagicSkillData) = {
        name = x.name
        damageTypes = x.damageTypes
        isMeleeCapable = x.isMeleeCapable
        isRangeCapable = x.isRangeCapable
    }

    type LiteDB_MagicSystem = {
        name: string
        vocationName: string
        vocationGoverningAttributeSet: AttributeName seq
        resourceName: string
        governingCoreSkill: string
        magicSkillDataSet: LiteDB_MagicSkillData seq
    }

    let toMagicSystem x : MagicSystem = {
        name = x.name
        vocationName = x.vocationName
        vocationGoverningAttributeSet = Set.ofSeq x.vocationGoverningAttributeSet
        resourceName = x.resourceName
        governingCoreSkill = x.governingCoreSkill
        magicSkillDataSet = x.magicSkillDataSet |> Seq.map toMagicSkillData |> Set.ofSeq
    }

    let toLiteDB_MagicSystem (x: MagicSystem) = {
        name = x.name
        vocationName = x.vocationName
        vocationGoverningAttributeSet = x.vocationGoverningAttributeSet
        resourceName = x.resourceName
        governingCoreSkill = x.governingCoreSkill
        magicSkillDataSet = x.magicSkillDataSet |> Seq.map toLiteDB_MagicSkillData
    }

    type LiteDB_MagicVocationSkill =
        | MagicSkill of LiteDB_Skill
        | MundaneVocationSkill of LiteDB_MundaneVocationSkill

    let toMagicVocationSkill =
        function
        | MagicSkill skill -> toSkill skill |> MagicVocationSkill.MagicSkill
        | MundaneVocationSkill skill -> toMundaneVocationSkill skill |> MagicVocationSkill.MundaneVocationSkill

    let toLiteDB_MagicVocationSkill =
        function
        | MagicVocationSkill.MagicSkill skill -> toLiteDB_Skill skill |> MagicSkill
        | MagicVocationSkill.MundaneVocationSkill skill -> toLiteDB_MundaneVocationSkill skill |> MundaneVocationSkill

    type LiteDB_MagicVocationExtras = {
        magicVocationSkills: LiteDB_MagicVocationSkill seq
        magicSystem: LiteDB_MagicSystem
        vocationResourcePool: uint
        coreSkillResourcePool: uint
        currentMagicResource: uint
    }

    let toMagicVocationExtras x : MagicVocationExtras = {
        magicVocationSkills = x.magicVocationSkills |> Seq.map toMagicVocationSkill |> Set.ofSeq
        magicSystem = toMagicSystem x.magicSystem
        vocationResourcePool = x.vocationResourcePool
        coreSkillResourcePool = x.coreSkillResourcePool
        currentMagicResource = x.currentMagicResource
    }

    let toLiteDB_MagicVocationExtras (x: MagicVocationExtras) = {
        magicVocationSkills = x.magicVocationSkills |> Seq.map toLiteDB_MagicVocationSkill
        magicSystem = toLiteDB_MagicSystem x.magicSystem
        vocationResourcePool = x.vocationResourcePool
        coreSkillResourcePool = x.coreSkillResourcePool
        currentMagicResource = x.currentMagicResource
    }

    type LiteDB_MundaneOrMagicVocationExtras =
        | MundaneVocationExtras of LiteDB_MundaneVocationSkill seq
        | MagicVocationExtras of LiteDB_MagicVocationExtras

    let toMundaneOrmagicVocationExtras =
        function
        | MundaneVocationExtras skills ->
            skills
            |> Seq.map toMundaneVocationSkill
            |> Set.ofSeq
            |> MundaneOrMagicVocationExtras.MundaneVocationExtras
        | MagicVocationExtras magicVocationExtras ->
            magicVocationExtras
            |> toMagicVocationExtras
            |> MundaneOrMagicVocationExtras.MagicVocationExtras

    let toLiteDB_MundaneOrmagicVocationExtras =
        function
        | MundaneOrMagicVocationExtras.MundaneVocationExtras skills ->
            skills |> Seq.map toLiteDB_MundaneVocationSkill |> MundaneVocationExtras
        | MundaneOrMagicVocationExtras.MagicVocationExtras magicVocationExtras ->
            magicVocationExtras |> toLiteDB_MagicVocationExtras |> MagicVocationExtras

    type LiteDB_Vocation = {
        vocationStat: LiteDB_VocationStat
        mundaneOrMagicVocationExtras: LiteDB_MundaneOrMagicVocationExtras
    }

    let toVocation x : Vocation = {
        vocationStat = toVocationStat x.vocationStat
        mundaneOrMagicVocationExtras = toMundaneOrmagicVocationExtras x.mundaneOrMagicVocationExtras
    }

    let toLiteDB_Vocation (x: Vocation) = {
        vocationStat = toLiteDB_VocationStat x.vocationStat
        mundaneOrMagicVocationExtras = toLiteDB_MundaneOrmagicVocationExtras x.mundaneOrMagicVocationExtras
    }

    type LiteDB_CombatRoll = {
        itemName: string
        weaponTypeName: string
        handedVariation: string
        resourceName: string
        resourceDicePoolMod: DicePoolMod
        dicePool: DicePool
        weaponandOffhandDicePoolModString: DicePoolMod
        calculatedRange: CalculatedRange
        penetration: Penetration
        damageTypeSet: DamageType seq
        setAreaOfEffectOption: SetAreaOfEffect Option
        calculatedEngageableOpponents: CalculatedEngageableOpponents
        eoName: string option
    }

    let toCombatRoll x : CombatRoll = {
        itemName = x.itemName
        weaponTypeName = x.weaponTypeName
        handedVariation = x.handedVariation
        resourceName = x.resourceName
        resourceDicePoolMod = x.resourceDicePoolMod
        dicePool = x.dicePool
        weaponandOffhandDicePoolModString = x.weaponandOffhandDicePoolModString
        calculatedRange = x.calculatedRange
        penetration = x.penetration
        damageTypeSet = x.damageTypeSet |> Set.ofSeq
        setAreaOfEffectOption = x.setAreaOfEffectOption
        calculatedEngageableOpponents = x.calculatedEngageableOpponents
        eoName = x.eoName
    }

    let toLiteDB_CombatRoll (x: CombatRoll) = {
        itemName = x.itemName
        weaponTypeName = x.weaponTypeName
        handedVariation = x.handedVariation
        resourceName = x.resourceName
        resourceDicePoolMod = x.resourceDicePoolMod
        dicePool = x.dicePool
        weaponandOffhandDicePoolModString = x.weaponandOffhandDicePoolModString
        calculatedRange = x.calculatedRange
        penetration = x.penetration
        damageTypeSet = x.damageTypeSet
        setAreaOfEffectOption = x.setAreaOfEffectOption
        calculatedEngageableOpponents = x.calculatedEngageableOpponents
        eoName = x.eoName
    }

    type LiteDB_WeightClass = {
        name: string
        bottomPercentOption: float option
        topPercentOption: float option
        attributeDeterminedDiceModEffect: LiteDB_AttributeDeterminedDiceMod
    }

    let toWeightClass x : WeightClass = {
        name = x.name
        bottomPercentOption = x.bottomPercentOption
        topPercentOption = x.bottomPercentOption
        attributeDeterminedDiceModEffect = toAttributeDeterminedDiceMod x.attributeDeterminedDiceModEffect
    }

    let toLiteDB_WeightClass (x: WeightClass) = {
        name = x.name
        bottomPercentOption = x.bottomPercentOption
        topPercentOption = x.bottomPercentOption
        attributeDeterminedDiceModEffect = toLiteDB_AttributeDeterminedDiceMod x.attributeDeterminedDiceModEffect
    }

    type LiteDB_SettingData = {
        attributeNameSet: AttributeName seq
        coreSkillDataSet: CoreSkillData seq
        itemElementSet: LiteDB_ItemElement seq
        weaponSpellSet: WeaponSpell seq
        magicSystemSet: LiteDB_MagicSystem seq
        weaponSkillDataSet: LiteDB_WeaponSkillData seq
        effectSet: LiteDB_Effect seq
        combatSpeedCalculationSet: CombatSpeedCalculation seq
        carryWeightCalculationSet: CarryWeightCalculation seq
        weightClassSet: LiteDB_WeightClass seq
    }

    let toSettingData x : SettingData = {
        attributeNameSet = Set.ofSeq x.attributeNameSet
        coreSkillDataSet = Set.ofSeq x.coreSkillDataSet
        itemElementSet = x.itemElementSet |> Seq.map toItemElement |> Set.ofSeq
        weaponSpellSet = Set.ofSeq x.weaponSpellSet
        magicSystemSet = x.magicSystemSet |> Seq.map toMagicSystem |> Set.ofSeq
        weaponSkillDataSet = x.weaponSkillDataSet |> Seq.map toWeaponSkillData |> Set.ofSeq
        effectSet = x.effectSet |> Seq.map toEffect |> Set.ofSeq
        combatSpeedCalculationSet = Set.ofSeq x.combatSpeedCalculationSet
        carryWeightCalculationSet = Set.ofSeq x.carryWeightCalculationSet
        weightClassSet = x.weightClassSet |> Seq.map toWeightClass |> Set.ofSeq
    }

    let toLiteDB_SettingData (x: SettingData) = {
        attributeNameSet = x.attributeNameSet
        coreSkillDataSet = x.coreSkillDataSet
        itemElementSet = x.itemElementSet |> Seq.map toLiteDB_ItemElement
        weaponSpellSet = x.weaponSpellSet
        magicSystemSet = x.magicSystemSet |> Seq.map toLiteDB_MagicSystem
        weaponSkillDataSet = x.weaponSkillDataSet |> Seq.map toLiteDB_WeaponSkillData
        effectSet = x.effectSet |> Seq.map toLiteDB_Effect
        combatSpeedCalculationSet = x.combatSpeedCalculationSet
        carryWeightCalculationSet = x.carryWeightCalculationSet
        weightClassSet = x.weightClassSet |> Seq.map toLiteDB_WeightClass
    }

    type LiteDB_Character = {
        id: int
        name: string
        attributes: Attribute seq
        coreSkills: LiteDB_Skill seq
        destinyPoints: ZeroToThree
        vocationList: LiteDB_Vocation list
        equipment: LiteDB_ItemElement List
        combatRollList: LiteDB_CombatRoll List
        characterInformation: CharacterInformation
        characterEffects: LiteDB_Effect List
        combatSpeeds: CombatSpeed List
        weightClassOption: LiteDB_WeightClass option
        carryWeightCalculationOption: CarryWeightCalculation option
    }

    let toCharacter x : Character = {
        id = x.id
        name = x.name
        attributes = Set.ofSeq x.attributes
        coreSkills = x.coreSkills |> Seq.map toSkill |> Set.ofSeq
        destinyPoints = x.destinyPoints
        vocationList = x.vocationList |> List.map toVocation
        equipment = x.equipment |> List.map toItemElement
        combatRollList = x.combatRollList |> List.map toCombatRoll
        characterInformation = x.characterInformation
        characterEffects = x.characterEffects |> List.map toEffect
        combatSpeeds = x.combatSpeeds
        weightClassOption =
            match x.weightClassOption with
            | None -> None
            | Some weightClass -> weightClass |> toWeightClass |> Some
        carryWeightCalculationOption = x.carryWeightCalculationOption
    }

    let toLiteDB_Character (x: Character) = {
        id = x.id
        name = x.name
        attributes = x.attributes
        coreSkills = x.coreSkills |> Seq.map toLiteDB_Skill
        destinyPoints = x.destinyPoints
        vocationList = x.vocationList |> List.map toLiteDB_Vocation
        equipment = x.equipment |> List.map toLiteDB_ItemElement
        combatRollList = x.combatRollList |> List.map toLiteDB_CombatRoll
        characterInformation = x.characterInformation
        characterEffects = x.characterEffects |> List.map toLiteDB_Effect
        combatSpeeds = x.combatSpeeds
        weightClassOption =
            match x.weightClassOption with
            | None -> None
            | Some weightClass -> weightClass |> toLiteDB_WeightClass |> Some
        carryWeightCalculationOption = x.carryWeightCalculationOption
    }

    [<CLIMutableAttribute>]
    type LiteDB_Setting = {
        id: int
        name: string
        characters: LiteDB_Character List // This has to be a List here, that we will never delete from to mimic database insertion
        SettingData: LiteDB_SettingData
    }

    let toSetting (x: LiteDB_Setting) : Setting =

        {
            id = x.id
            name = x.name
            characters = x.characters |> Seq.map toCharacter
            SettingData = x.SettingData |> toSettingData
        }

    let toLiteDB_Setting (x: Setting) = {
        id = x.id
        name = x.name
        characters = x.characters |> Seq.map toLiteDB_Character |> List.ofSeq
        SettingData = x.SettingData |> toLiteDB_SettingData
    }

let db =
    let mapper = FSharpBsonMapper()
    let connStr = "Filename=fogentData.db;mode=Exclusive"
    new LiteDatabase(connStr, mapper)

let collectionFromDB<'T> = db.GetCollection<'T>(typeof<'T>.Name)

[<CLIMutable>]
type UserCharacterAccess = {
    Id: int
    UserId: int
    SettingId: int
    CharacterId: int
}

let users: LiteCollection<IdUser> = collectionFromDB<IdUser>
let liteDB_Settings = collectionFromDB<LiteDB_Setting>
let userCharacterAccesses = collectionFromDB<UserCharacterAccess>

[<AutoOpenAttribute>]
module UserCharacterAccessFilters =

    // UCA filter Utils
    let isUserIdInUca userId uca = uca.UserId = userId

    let isSettingIdInUca settingId uca = uca.SettingId = settingId

    let isUserIdAndSettingIdInUca userId settingId uca =
        (uca.UserId = userId) && (uca.SettingId = settingId)

    let filterUcaByUserId userId =
        // This one is kinda worthless as I would never need to grab all characters without reference to what setting they are in
        userCharacterAccesses.Find(isUserIdInUca userId)

    let filterUcaBySettingId settingId =
        userCharacterAccesses.Find(isSettingIdInUca settingId)

    let filterUcaByUserIdAndSettingId userId settingId =
        userCharacterAccesses.Find(isUserIdAndSettingIdInUca userId settingId)

    let filterUcaBySettingIdAndCharacterId settingId characterId =
        // Even though unused now, could be handy later for grabing all users that have access to a character
        settingId
        |> filterUcaBySettingId
        |> Seq.filter (fun uca -> uca.CharacterId = characterId)

[<AutoOpenAttribute>]
module LiteDBCollectionTryFinds =

    let tryUsernameToUser (username: Username) =
        users.Find(fun idUser -> idUser.Login.username = username) |> Seq.tryHead

    let tryFindIdUser userId =
        if users.Exists((fun user -> user.Id = userId)) then
            users.FindById(BsonValue(userId)) |> Some
        else
            None

    let tryFindSetting liteDB_SettingId =
        if liteDB_Settings.Exists((fun x -> x.id = liteDB_SettingId)) then
            liteDB_Settings.FindById(BsonValue(liteDB_SettingId)) |> Some
        else
            None

    let tryFindCharacter (characterId: int) (setting: LiteDB_Setting) =
        setting.characters |> List.tryFind (fun character -> character.id = characterId)

    let tryFindCharacterWithCharacterIdAndSettingId settingId characterId =
        settingId
        |> tryFindSetting
        |> function
            | None -> None
            | Some liteDB_Setting -> tryFindCharacter characterId liteDB_Setting

    let tryPrimativeUca userId settingId characterId =
        let userOption = tryFindIdUser userId

        let characterOption =
            tryFindCharacterWithCharacterIdAndSettingId settingId characterId

        match userOption, characterOption with
        | Some user, Some character -> Some(user, character)
        | _, _ -> None

    let tryPrimativeUcaToLiteDB_Character userId settingId characterId =
        match tryPrimativeUca userId settingId characterId with
        | Some(user, character) -> Some character
        | None -> None

    let tryUca uca =
        tryPrimativeUca uca.UserId uca.SettingId uca.CharacterId

    let tryUcaToLiteDB_Character uca =
        match tryUca uca with
        | Some(user, character) -> Some character
        | None -> None


    let tryUcaToUser uca =
        match tryUca uca with
        | Some(user, character) -> Some user
        | None -> None

[<AutoOpenAttribute>]
module UserCharacterAccessUtils =

    let tryUserIdAndSettingIdToOwnedSetting userId settingId =

        settingId
        |> tryFindSetting
        |> Option.map (fun liteDB_Setting ->

            let setting = liteDB_Setting |> toSetting

            {
                setting with
                    characters =
                        filterUcaByUserIdAndSettingId userId settingId
                        |> Seq.choose tryUcaToLiteDB_Character
                        |> Seq.map toCharacter
            })

    let userIdToOwnedSettings userId : Setting seq =

        liteDB_Settings.FindAll()
        |> Seq.choose (fun setting -> tryUserIdAndSettingIdToOwnedSetting userId setting.id)


    let settingIdAndCharacterIdToUsers settingId characterId =
        filterUcaBySettingIdAndCharacterId settingId characterId
        |> Seq.choose tryUcaToUser

[<AutoOpenAttribute>]
module LiteDbTryInserts =

    let tryInsertNewUser (user: Login) =
        try
            users.Insert({ Id = 0; Login = user }) |> ignore |> Ok
        with _ ->
            Error("Failed to insert new user.")

    let tryInsertNewUserCharacterAccess userId settingId characterId =
        try
            userCharacterAccesses.Insert(
                {
                    Id = 0
                    UserId = userId
                    CharacterId = characterId
                    SettingId = settingId
                }
            )
            |> ignore
            |> Ok
        with _ ->
            Error("Failed to insert new user character access")

    let tryInsertLiteDB_Setting (updatedLiteDB_Setting: LiteDB_Setting) =
        try
            liteDB_Settings.Insert(updatedLiteDB_Setting) |> Some
        with _ ->
            None

    let tryUpdateLiteDB_Setting (updatedLiteDB_Setting: LiteDB_Setting) =
        match liteDB_Settings.Update(updatedLiteDB_Setting) with
        | true -> Some true
        | false -> None

    let innerInsertNewCharacter userId (settingId: int) =

        // Might wanna replace these with result at some point
        tryFindSetting settingId
        |> Option.map (fun liteDB_Setting ->
            liteDB_Setting.SettingData
            |> toSettingData
            |> FogentRoleplayLib.Character.init liteDB_Setting.characters.Length,
            liteDB_Setting)
        |> Option.bind (fun (newCharacter, liteDB_Setting) ->
            {
                liteDB_Setting with
                    characters =
                        newCharacter
                        |> toLiteDB_Character
                        |> List.singleton
                        |> List.append liteDB_Setting.characters
            }
            |> tryInsertLiteDB_Setting
            |> Option.bind (fun _ ->
                try
                    tryInsertNewUserCharacterAccess userId liteDB_Setting.id newCharacter.id |> Some
                with _ ->
                    None)
            |> Option.map (fun _ -> newCharacter))

    let insertNewCharacterInSettingForUser username settingId =

        settingId
        |> tryFindSetting
        |> Option.map (fun liteDB_Setting ->
            liteDB_Setting.SettingData
            |> toSettingData
            |> FogentRoleplayLib.Character.init liteDB_Setting.characters.Length,
            liteDB_Setting)
        |> Option.bind (fun (newCharacter, liteDB_Setting) ->
            tryUsernameToUser username
            |> Option.map (fun idUser -> (newCharacter, liteDB_Setting, idUser)))
        |> Option.bind (fun (newCharacter, liteDB_Setting, idUser) ->
            {
                liteDB_Setting with
                    characters =
                        newCharacter
                        |> toLiteDB_Character
                        |> List.singleton
                        |> List.append liteDB_Setting.characters
            }
            |> tryUpdateLiteDB_Setting
            |> Option.bind (fun _ ->
                try
                    tryInsertNewUserCharacterAccess idUser.Id liteDB_Setting.id newCharacter.id
                    |> Some
                with _ ->
                    None)
            |> Option.map (fun _ -> newCharacter))
        |> function
            | None -> Error("Failed to insert new character in setting.")
            | Some character -> Ok(character)

[<AutoOpenAttribute>]
module LiteDbTryUpdates =

    let updateCharacter username settingId (newCharacter: Character) =

        username
        |> tryUsernameToUser
        |> Option.bind (fun idUser -> tryPrimativeUcaToLiteDB_Character idUser.Id settingId newCharacter.id)
        |> Option.bind (fun _ -> settingId |> tryFindSetting)
        |> Option.map (fun liteDB_Setting -> {
            liteDB_Setting with
                characters =
                    liteDB_Setting.characters
                    |> List.insertAt newCharacter.id (toLiteDB_Character newCharacter)
        })
        |> Option.bind tryInsertLiteDB_Setting
        |> function
            | None -> Error("Failed to update character.")
            | Some _ -> Ok()

let isValidUserLogin (login: Login) =

    users.Find(
        Query.And(
            Query.EQ("Login.username", BsonValue(login.username)),
            Query.EQ("Login.password", BsonValue(login.password))
        )
    )
    |> Seq.tryHead
    |> (function
    | Some _ -> true
    | None ->
        tryInsertNewUser login // TESTING, REMOVE ASAP: This automatically creates a user if it doesn't exists,
        false)

open FogentRoleplayLib.Setting

// {
//     SettingData = CsvDatabase.getInitSettingDataFromCSV ()
//     characters = Seq.empty
//     id = 0
//     name = "Fallen"
// }
// |> toLiteDB_Setting
// |> tryInsertLiteDB_Setting