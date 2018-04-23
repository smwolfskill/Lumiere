using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

[CreateAssetMenu(menuName = "Lumiere/Room/Secret")]
public class SecretRoomType : RoomType
{
    public TileType sandTile;
    public TileType wallTile;
    public float itemPadding = 1.5f;         //dist from secret room walls
    public float healthPotionChance = 0.5f;  //TODO: update later w/ difficulty, etc.

    public int minLootCap = 5; //cap on min #loot items in a room, regardless of level & difficulty.
    public int maxLootCap = 8; //cap on max #loot items in a room, regardless of level & difficulty.
    public int levelsTillMinItemGain_easiest = 10;          //number of levels taken to increase minItems by 1, on easiest difficulty.
    public float levelsTillMinItemGain_multiplier = 1.4f;  //multiplier of levelsTillMinItemGain_easiest per difficulty above easiest.
    public int levelsTillMaxItemGain_easiest = 5;          //number of levels taken to increase maxItems by 1, on easiest difficulty.
    public float levelsTillMaxItemGain_multiplier = 1.4f;  //multiplier of levelsTillMaxItemGain_easiest per difficulty above easiest.
    public int minItemsBase_hardest = 0;                   //minItems on lowest level at hardest difficulty
    public float minItemsBase_modifier = 0.6f;             //amount to increase minItemsBase_hardest by for each easier difficulty.
    public int maxItemsBase_hardest = 1;                   //maxItems on lowest level at hardest difficulty
    public float maxItemsBase_modifier = 0.4f;             //amount to increase maxItemsBase_hardest by for each easier difficulty.
    public GameItem.ItemRarity minRarityCap = (GameItem.ItemRarity) Enum.GetValues(typeof(GameItem.ItemRarity)).Length - 1;
    public GameItem.ItemRarity minRarityBase_hardest = (GameItem.ItemRarity) 0;
    public float minRarityBase_modifier = 0.4f;
    public int levelsTillMinRarityGain_easiest = 7;
    public float levelsTillMinRarityGain_multiplier = 1.4f;

    //Difficulty: 0 (easiest) -> higher (harder)

    public override void GenRoom(Room room, Map map)
    {
        map.FillAreaWithBorder(room.x, room.y, room.w, room.h, sandTile, wallTile, room);

        //Set loot parameters depending on level & difficulty
        int maxDifficulty = Enum.GetValues(typeof(Settings.Difficulty)).Length - 1;

        int minItems_base = Mathf.RoundToInt(minItemsBase_hardest + minItemsBase_modifier * (maxDifficulty - map.difficulty));
        int maxItems_base = Mathf.RoundToInt(maxItemsBase_hardest + maxItemsBase_modifier * (maxDifficulty - map.difficulty));
        //Debug.Log("minBase = " + (minItemsBase_hardest + minItemsBase_modifier * (maxDifficulty - map.difficulty)).ToString() + "; " + maxItems_base.ToString());
        int levelsTillMinItemGain = Mathf.RoundToInt(levelsTillMinItemGain_easiest * Mathf.Pow(levelsTillMinItemGain_multiplier, map.difficulty));
        int levelsTillMaxItemGain = Mathf.RoundToInt(levelsTillMaxItemGain_easiest * Mathf.Pow(levelsTillMaxItemGain_multiplier, map.difficulty));
        int minItems_gain = (int) map.levelNumber / levelsTillMinItemGain;
        int maxItems_gain = (int) map.levelNumber / levelsTillMaxItemGain;
        //Debug.Log("minGain = " + minItems_gain.ToString() + "; " + maxItems_gain.ToString());
        int minItems = Mathf.Min(minItems_base + minItems_gain, minLootCap);
        int maxItems = Mathf.Min(maxItems_base + maxItems_gain, maxLootCap);
        if(minItems > maxItems)
        {
            minItems = maxItems;
        }

        //Rarity
        int minRarityBase = Mathf.RoundToInt((int) minRarityBase_hardest + minRarityBase_modifier * (maxDifficulty - map.difficulty));
        int levelsTillMinRarityGain = Mathf.RoundToInt(levelsTillMinRarityGain_easiest * Mathf.Pow(levelsTillMinRarityGain_multiplier, map.difficulty));
        int minRarity_gain = (int) map.levelNumber / levelsTillMinRarityGain;
        GameItem.ItemRarity minRarity = (GameItem.ItemRarity) Mathf.Min(minRarityBase + minRarity_gain, (int) minRarityCap);
        //Debug.Log("minRarity = " + minRarity.ToString() + "; base = " + minRarityBase.ToString() + "; gain = " + minRarity_gain.ToString());
        //Debug.Log("(l, d) = (" + map.levelNumber + ", " + map.difficulty + "); (minItems, maxItems) = (" + minItems.ToString() + ", " + maxItems.ToString() + ")");


        //Generate loot in secret room
        GameItem[] loot = ItemSpawner.GenerateLootBag((int)Time.time, map.levelNumber, minItems, maxItems, minRarity, false, healthPotionChance);
        if(loot.Length == 0)
        {
            return;
        }
        if(loot.Length == 1)
        {
            //Spawn in center of room
            loot[0].CreateGameObject(new Vector3(room.centerX, room.centerY, 0));
            //Debug.Log("Spawned 1 item in center of secret room");
        }
        else //use multiples of 4 scaling
        {
            int dim = (int) Mathf.Floor((loot.Length + 3) / 4) + 1; //dimension of square used to place items on border of
            float usableRoomWidth = room.w - 1.5f * itemPadding;
            float usableRoomHeight = room.h - 1.5f * itemPadding;
            float itemSpacing_x = usableRoomWidth / dim;
            float itemSpacing_y = usableRoomHeight / dim;

            int i = 0;
            for(int y = 0; y < dim; y++)
            {
                if(i == loot.Length)
                {
                    break;
                }
                for(int x = 0; x < dim; x++)
                {
                    if(i == loot.Length)
                    {
                        break;
                    }
                    if(x == 0 || x == (dim - 1) || y == 0 || y == (dim - 1)) //along edge of square: spawn item
                    {
                        //(avoid spawning items in middle of square unless only 1 item)
                        float itemX = room.x + itemPadding + x*itemSpacing_x;
                        float itemY = room.y + itemPadding + y*itemSpacing_y;
                        loot[i].CreateGameObject(new Vector3(itemX, itemY, 0.0f));
                        i++;
                    }
                }
            }
        }
    }

}
