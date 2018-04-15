using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// Specialized static class designed to handle item generation algorithms.
/// </summary>
static class ItemSpawner
{
    #region Item Generators
    /// <summary>
    /// Generation function designed to build an equippable item for spawning, can be a weapon or armor.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>An equippable item, with name and description pre-set based on the item spec.</returns>
    public static EquippableItem GenerateEquipment(int seed, int quality, GameItem.ItemRarity rarity = GameItem.ItemRarity.COMMON)
    {
        // TODO
        return null;
    }

    /// <summary>
    /// Generation function designed to build an armor item for spawning. Can fit any slot.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>A weapon item, with name and description pre-set based on the item spec.</returns>
    public static ArmorItem GenerateArmor(int seed, int quality, GameItem.ItemRarity rarity = GameItem.ItemRarity.COMMON)
    {
        // TODO
        return null;
    }

    /// <summary>
    /// Generation function designed to build a weapon item of some sort. Can be a melee or ranged weapon.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>A weapon item, with name and description pre-set based on the item spec.</returns>
    public static WeaponItem GenerateWeapon(int seed, int quality, GameItem.ItemRarity rarity = GameItem.ItemRarity.COMMON)
    {
        // TODO
        return null;
    }

    /// <summary>
    /// Generation function designed to build specifically a melee weapon.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>A melee weapon with pre-set name and description.</returns>
    public static MeleeWeapon GenerateMeleeWeapon(int seed, int quality, GameItem.ItemRarity rarity = GameItem.ItemRarity.COMMON)
    {
        // TODO
        return null;
    }

    /// <summary>
    /// Generation function designed to build specifically a ranged weapon.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>A ranged weapon with pre-set name and description.</returns>
    public static RangedWeapon GenerateRangedWeapon(int seed, int quality, GameItem.ItemRarity rarity = GameItem.ItemRarity.COMMON)
    {
        // TODO
        return null;
    }

    /// <summary>
    /// Generation function designed to build an array of game items, can be potions, equipment, or just misc stuff. 
    /// Function is primarily used for spawning more than one thing in a treasure room.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="min">Minimum amount of items that are allowed to spawn.</param>
    /// <param name="max">Maximum amount of items that are allowed to spawn.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>An equippable item, with name and description pre-set based on the item spec.</returns>
    public static GameItem[] GenerateLootBag(int seed, int quality, int min = 1, int max = 5, GameItem.ItemRarity rarity = GameItem.ItemRarity.COMMON)
    {
        // TODO
        return null;
    }
    #endregion

    #region Additional Utility Functions
    /// <summary>
    /// Takes a weapon and generates a name based on the item's stats.
    /// </summary>
    /// <param name="seed">Seed for generating the name. Should be set to system time during live gameplay.</param>
    /// <param name="item">The item data to find a name for. Name should fit the data.</param>
    /// <returns>A potential name for the item.</returns>
    public static string GenerateWeaponName(int seed, WeaponItem item)
    {
        // TODO
        return "";
    }

    /// <summary>
    /// Takes a piece of armor and generates a name based on the item's stats.
    /// </summary>
    /// <param name="seed">Seed for generating the name. Should be set to system time during live gameplay.</param>
    /// <param name="item">The item data to find a name for. Name should fit the data.</param>
    /// <returns>A potential name for the item.</returns>
    public static string GenerateArmorName(int seed, ArmorItem item)
    {
        // TODO
        return "";
    }

    /// <summary>
    /// Takes a weapon and generates a description based on the item's stats.
    /// Requires a name to function correctly.
    /// </summary>
    /// <param name="seed">Seed for generating the description. Should be set to system time during live gameplay.</param>
    /// <param name="item">The item data to find a description for. Description should fit the data.</param>
    /// <returns>A potential description for the item.</returns>
    public static string GenerateWeaponDesc(int seed, WeaponItem item)
    {
        // TODO
        return "";
    }

    /// <summary>
    /// Takes a piece of armor and generates a description based on the item's stats.
    /// Requires a name to function correctly.
    /// </summary>
    /// <param name="seed">Seed for generating the description. Should be set to system time during live gameplay.</param>
    /// <param name="item">The item data to find a description for. Description should fit the data.</param>
    /// <returns>A potential description for the item.</returns>
    public static string GenerateArmorDesc(int seed, ArmorItem item)
    {
        // TODO
        return "";
    }

    /// <summary>
    /// A function designed to determine wether or not a monster should drop an item.
    /// </summary>
    /// <param name="seed">Seed for generating the bool. Should be set to system time during live gameplay.</param>
    /// <param name="rewardFactor">A single integer representing the likelyhood this particular enemy should drop a reward. Ideally based both on floor level and monster power.</param>
    /// <returns></returns>
    public static bool ShouldDropItem(int seed, int rewardFactor)
    {
        // TODO
        return false;
    }
    #endregion
}
