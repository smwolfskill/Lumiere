using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
public class Settings
{
    public enum Difficulty
    {
        Easy,
        Normal,
        Hard
    }

    public Difficulty difficulty;
    public string moveUp;
    public string moveDown;
    public string moveLeft;
    public string moveRight;
    public string useItem;
    public string dropItem;
    public string pickupItem;
    public string stackModifier;
    public string openInventory;

    public Settings()
    {
        difficulty = Difficulty.Easy;
        moveUp = "W";
        moveDown = "S";
        moveLeft = "A";
        moveRight = "D";
        useItem = "E";
        dropItem = "Q";
        pickupItem = "Mouse 0";
        stackModifier = "Shift";
        openInventory = "Tab";
    }

}
