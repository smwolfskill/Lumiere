using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

/// <summary>
/// Holds fields governing type, quality and quantity of loot items to be dropped by entities or spawned in secret rooms.
/// </summary>
[System.Serializable]
public class EntityDropGen
{
    public float healthPotionChance = 0.5f;

    [Header("Loot Caps, regardless of level & difficulty")]
    public int minLootCap = 5; //cap on min #loot items in a room, regardless of level & difficulty.
    public int maxLootCap = 8; //cap on max #loot items in a room, regardless of level & difficulty.
    public GameItem.ItemRarity minRarityCap = (GameItem.ItemRarity)Enum.GetValues(typeof(GameItem.ItemRarity)).Length - 1;

    [Header("Base loot at hardest difficulty")]
    public GameItem.ItemRarity minRarityBase_hardest = (GameItem.ItemRarity)0;
    public int minItemsBase_hardest = 0;                   //minItems on lowest level at hardest difficulty
    public int maxItemsBase_hardest = 1;                   //maxItems on lowest level at hardest difficulty

    [Header("Linear modifiers on base loot for each difficulty below hardest")]
    public float minItemsBase_modifier = 0.6f;             //amount to increase minItemsBase_hardest by for each easier difficulty.
    public float maxItemsBase_modifier = 0.4f;             //amount to increase maxItemsBase_hardest by for each easier difficulty.
    public float minRarityBase_modifier = 0.4f;


    [Header("Levels Till Loot Increases on easiest difficulty")]
    public int levelsTillMinItemGain_easiest = 10;         //number of levels taken to increase minItems by 1, on easiest difficulty.
    public int levelsTillMaxItemGain_easiest = 5;          //number of levels taken to increase maxItems by 1, on easiest difficulty.
    public int levelsTillMinRarityGain_easiest = 7;

    [Header("Multipliers on Levels Till Loot Increases for each difficulty above easy")]
    public float levelsTillMinItemGain_multiplier = 1.4f;  //multiplier of levelsTillMinItemGain_easiest per difficulty above easiest.
    public float levelsTillMaxItemGain_multiplier = 1.4f;  //multiplier of levelsTillMaxItemGain_easiest per difficulty above easiest.
    public float levelsTillMinRarityGain_multiplier = 1.4f;

    private int levelNumber = 1;
    private float difficulty = 1.0f; //Difficulty: 0 (easiest) -> higher (harder)

    public void SetLevelAndDifficulty(Map map)
    {
        levelNumber = map.levelNumber;
        difficulty = map.difficulty;
    }

    /// <summary>
    /// Generates the loot within all specified parameters, depending on level & difficulty.
    /// </summary>
    /// <returns>The loot.</returns>
    /// <param name="map">Map to get the levelNumber and difficulty from. If unspecified uses set values. </param>
    public GameItem[] GenerateLoot(Map map = null)
    {
        if (map != null)
        {
            SetLevelAndDifficulty(map);
        }
        //Set loot parameters depending on level & difficulty
        int maxDifficulty = Enum.GetValues(typeof(Settings.Difficulty)).Length - 1;

        int minItems_base = Mathf.RoundToInt(minItemsBase_hardest + minItemsBase_modifier * (maxDifficulty - difficulty));
        int maxItems_base = Mathf.RoundToInt(maxItemsBase_hardest + maxItemsBase_modifier * (maxDifficulty - difficulty));
        int levelsTillMinItemGain = Mathf.RoundToInt(levelsTillMinItemGain_easiest * Mathf.Pow(levelsTillMinItemGain_multiplier, difficulty));
        int levelsTillMaxItemGain = Mathf.RoundToInt(levelsTillMaxItemGain_easiest * Mathf.Pow(levelsTillMaxItemGain_multiplier, difficulty));
        int minItems_gain = (int)levelNumber / levelsTillMinItemGain;
        int maxItems_gain = (int)levelNumber / levelsTillMaxItemGain;
        int minItems = Mathf.Min(minItems_base + minItems_gain, minLootCap);
        int maxItems = Mathf.Min(maxItems_base + maxItems_gain, maxLootCap);
        if (minItems > maxItems)
        {
            minItems = maxItems;
        }

        //Rarity
        int minRarityBase = Mathf.RoundToInt((int)minRarityBase_hardest + minRarityBase_modifier * (maxDifficulty - difficulty));
        int levelsTillMinRarityGain = Mathf.RoundToInt(levelsTillMinRarityGain_easiest * Mathf.Pow(levelsTillMinRarityGain_multiplier, difficulty));
        int minRarity_gain = (int)levelNumber / levelsTillMinRarityGain;
        GameItem.ItemRarity minRarity = (GameItem.ItemRarity)Mathf.Min(minRarityBase + minRarity_gain, (int)minRarityCap);

        //Generate loot in secret room
        return ItemSpawner.GenerateLootBag((int)Time.time, levelNumber, minItems, maxItems, minRarity, false, healthPotionChance);
    }
}
