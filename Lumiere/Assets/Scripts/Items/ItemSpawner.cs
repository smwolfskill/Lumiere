using System.Collections;
using System.Collections.Generic;
using System;

/// <summary>
/// Specialized static class designed to handle item generation algorithms.
/// </summary>
static class ItemSpawner
{
    // Essentially a const, it lists the liklyhood an item will be a certain rarity or higher based on the scale.
    private static readonly int[] rarityWeights = {20, 10, 5, 3, 1};

    private static int lastID = 100;

    #region Item Generators
    /// <summary>
    /// Generation function designed to build an either a weapon or armor.
    /// </summary>
    /// <param name="seed">Seed to use for generation, useful for testing. Set this to system time during playtime.</param>
    /// <param name="quality">Quality modifier for items. Essentially represents how deep in the dungeon the player is. Pass this from the map class ideally.</param>
    /// <param name="rarity">Minimum item rarity, defaults to common. Higher rarity items have better stats implicitly.</param>
    /// <returns>An item, with name and description pre-set based on the item spec.</returns>
    public static GameItem GenerateItem(int seed, int quality, GameItem.ItemRarity rarity = GameItem.ItemRarity.COMMON)
    {
        // Some constants to use for calculations:
        int armorWeight = 6;    // Weighted likelyhood to pick this over the other.
        int weaponWeight = 1;   // Weighted likelyhood to pick this over the other.

        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        // Choose what type of thing to generate, armor, or a weapon?
        int val = random.Next(1, armorWeight + weaponWeight + 1);

        // Call other generation functions based on this.
        if (val >= weaponWeight)
            return GenerateWeapon(seed, quality, rarity);
        else
            return GenerateArmor(seed, quality, rarity);
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
        // Some constants to use for calculations:
        double[] minArmorRarityRatings = {1, 2, 3, 4, 5};           // Scaling factor for minimum based on rarity. Min * RarityRating = Absolute Minimum
        int[] minArmorRatings = {5, 15, 25, 5, 1, 1};               // Minimum rating per armor slot, in enum order.
        int[] maxArmorRatings = {20, 50, 100, 10, 5, 5};            // Maximum rating per armor slot, in enum order.
        
        double[] minSpeedRarityRatings = {1, 1.25, 1.5, 1.75, 2};   // Scaling factor for minimum based on rarity.
        double[] minSpeedRatings = {1, 1, 1, 1, 1, 1};              // Minimum rating per armor slot, in enum order.
        double[] maxSpeedRatings = {2, 5, 2, 1, 5, 5};              // Maximum rating per armor slot, in enum order.

        double[] minDamageRarityRatings = {1, 1.25, 1.5, 1.75, 2};  // Scaling factor for minimum based on rarity.
        double[] minDamageRatings = {1, 1, 1, 1, 1, 1};             // Minimum rating per armor slot, in enum order.
        double[] maxDamageRatings = {2, 2, 2, 5, 5, 5};             // Maximum rating per armor slot, in enum order. 

        // For reference, ENUM Order: HEAD, LEGS, CHEST, GLOVES, RING, NECK.

        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        // Select rarity first; this determines effectiveness.

        // Determine slot for armor.

        // Determine sprite info (somehow).

        // Calculate the armor's values.

        // Note that the quality modifier increases only the armor stat by a certain factor based on dungeon depth AFTER the value has already been calculated.
        // It hasn't exactly been determined HOW this impacts things; but it should be relative to the monster's attack damage and health at a given depth.

        // Build the armor item.

        // Generate a name.

        // Generate a description.

        // Pick a value rating. (Should probably be another utility method).

        // Generate a unique ID (this might require some internal memory somewhere to keep track of what IDs have already been assigned, probably another utility function).

        // Return the finished item.
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
        // Some constants to use for calculations:
        int meleeWeight = 1;    // Weighted likelyhood to pick this over the other.
        int rangedWeight = 0;   // Weighted likelyhood to pick this over the other.

        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        // Choose what type of thing to generate, armor, or a weapon?
        int val = random.Next(1, meleeWeight + rangedWeight);

        // Call other generation functions based on this.
        if (val >= rangedWeight)
            return GenerateMeleeWeapon(seed, quality, rarity);
        else
            return GenerateRangedWeapon(seed, quality, rarity);
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
        Random random = new Random(seed);
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
        Random random = new Random(seed);
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
        Random random = new Random(seed);
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
        // Use the other name generator functions based on what item we have.
        if (item is MeleeWeapon)
            return GenerateMeleeName(seed, item as MeleeWeapon);
        else
            return GenerateRangedName(seed, item as RangedWeapon);
    }

    private static string GenerateMeleeName(int seed, MeleeWeapon item)
    {
        // Name Pool.
        string[] types = {"Dagger", "Shortsword", "Longsword", "Two-Handed Sword"};

        // Based on the range.
        double[] typeCutoffRange = {1, 2, 3, 5};    

        // Prefixes, based on item rarity.
        string[] prefixesCommon = {"Basic", "Average", "Mundane", "Blunt", "Trivial", "Ugly"};
        string[] prefixesUncommon = {"Interesting", "Cool", "Sharp", "Pointy", "Stabby", "Zealous"};
        string[] prefixesRare = {"Exciting", "Strong", "True", "Knight's", "Rare", "Lordly"};
        string[] prefixesEpic = {"Powerful", "Dangerous", "Violent", "Epic", "Killer", "Unstoppable"};
        string[] prefixesLegend = {"Divine", "Legendary", "Godly", "Unbelivable", "Overpowered", "Mythical"};

        // Based on rarity, this is the likelyhood a suffix will appear on the weapon.
        int[] suffixThreshold = {90, 75, 40, 0, 0};            
        string[] suffixs = {"Fabulousness", "Coolness", "Swagger", "The Monk", "The Dragon", "The Burrito", "Dangerousness", "Pointyness", "Aggressiveness", "Revengence"};

        // Note we are using the system's random, and not Unity's in order to use a seed.
        Random random = new Random(seed);

        string prefix;
        bool suffixReq = false;

        // Pick a prefix and determine if a suffix is neccessary.
        switch (item.Rarity)
        {
            case GameItem.ItemRarity.UNCOMMON:
                prefix = prefixesUncommon[random.Next(0, prefixesUncommon.Length)];
                suffixReq = (random.Next(0, 101) >= suffixThreshold[1]);
                break;
            case GameItem.ItemRarity.RARE:
                prefix = prefixesRare[random.Next(0, prefixesRare.Length)];
                suffixReq = (random.Next(0, 101) >= suffixThreshold[2]);
                break;

            case GameItem.ItemRarity.EPIC:
                prefix = prefixesEpic[random.Next(0, prefixesEpic.Length)];
                suffixReq = (random.Next(0, 101) >= suffixThreshold[3]);
                break;

            case GameItem.ItemRarity.LEGENDARY:
                prefix = prefixesLegend[random.Next(0, prefixesLegend.Length)];
                suffixReq = (random.Next(0, 101) >= suffixThreshold[4]);
                break;

            default:
                prefix = prefixesCommon[random.Next(0, prefixesCommon.Length)];
                suffixReq = (random.Next(0, 101) >= suffixThreshold[0]);
                break;
        }

        // Generate the type;
        string type = "NOPE";
        double range = item.AttackRange;

        // Iterate through list of types, type is based on range.
        for (int i = 0; i < types.Length && type.CompareTo("NOPE") == 0; i++)
        {
            if (range <= typeCutoffRange[i])
            {
                type = types[i];
            }
        }

        // Pick a suffix (if there should be one).
        string suffix = "";

        if (suffixReq)
        {
            suffix = "of " + suffixs[random.Next(suffixs.Length)];
        }

        // Combine and return.
        return prefix + " " + type + " " + suffix;
    }

    private static string GenerateRangedName(int seed, RangedWeapon item)
    {
        return null;
    }

    /// <summary>
    /// Takes a piece of armor and generates a name based on the item's stats.
    /// </summary>
    /// <param name="seed">Seed for generating the name. Should be set to system time during live gameplay.</param>
    /// <param name="item">The item data to find a name for. Name should fit the data.</param>
    /// <returns>A potential name for the item.</returns>
    public static string GenerateArmorName(int seed, ArmorItem item)
    {
        Random random = new Random(seed);
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
        Random random = new Random(seed);
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
        Random random = new Random(seed);
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
        Random random = new Random(seed);
        return false;
    }

    /// <summary>
    /// Generates the value for a given item.
    /// </summary>
    /// <param name="item">Item data in question, will usually be passed weapons or equipment; should have private helper functions for those.</param>
    /// <returns>The value the item should logically have. Note it is not random.</returns>
    public static double GenerateItemValue(GameItem item)
    {
        return 0.0;
    }

    /// <summary>
    /// Generates an ID for a given item. This function is likely to require some sort of closure or variable that keeps track of what the max selected ID is, given the static nature of this class.
    /// I imagine the MIN ID to start at 100, so everything below that can be used for special items as needed for testing.
    /// </summary>
    /// <returns>An unused ID for the item.</returns>
    public static int GenerateItemID()
    {
        lastID++;
        return lastID;
    }
    #endregion
}
