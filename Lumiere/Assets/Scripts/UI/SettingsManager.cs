using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.IO;
using System;
//using NUnit.Framework;

public static class SettingsManager
{
    public static bool loaded = false;
    private static Settings settings;

    public static bool LoadSettings(string file_path) //TODO: main game loop load settings upon game launch
    {
        loaded = true;
        try
        {
            using (StreamReader r = new StreamReader(file_path))
            {
                string json = r.ReadToEnd();
                Settings loaded = JsonUtility.FromJson<Settings>(json);
                settings = loaded;
            }
            return true;
        } 
        catch 
        {
            Debug.Log("Setting default settings.");
            //Problem with loading from file, so set default settings.
            settings = new Settings();
            return false;
        }
    }

    public static bool SaveSettings(string file_path)
    {
        try
        {
            File.WriteAllText(file_path, JsonUtility.ToJson(settings, true));
            return true;
        }
        catch
        {
            return false;
        }
    }

    #region Getters
    public static KeyCode GetMoveUp()
    {
        return StringToKeyCode(settings.moveUp);
    }

    public static KeyCode GetMoveDown()
    {
        return StringToKeyCode(settings.moveDown);
    }

    public static KeyCode GetMoveLeft()
    {
        return StringToKeyCode(settings.moveLeft);
    }

    public static KeyCode GetMoveRight()
    {
        return StringToKeyCode(settings.moveRight);
    }

    public static KeyCode GetDropItem()
    {
        return StringToKeyCode(settings.dropItem);
    }

    public static KeyCode GetPickupItem()
    {
        return StringToKeyCode(settings.pickupItem);
    }

    public static KeyCode GetStackModifier()
    {
        return StringToKeyCode(settings.stackModifier);
    }

    public static KeyCode GetOpenInventory()
    {
        return StringToKeyCode(settings.openInventory);
    }

    public static KeyCode GetUseItem()
    {
        return StringToKeyCode(settings.useItem);
    }
    public static Settings.Difficulty GetDifficulty()
    {
        return settings.difficulty;
    }
    #endregion
    
    public static KeyCode GetKey(string settingName)
    {
        switch(settingName)
        {
            case "moveUp": return GetMoveUp();
                break;
            case "moveDown": return GetMoveDown();
                break;
            case "moveLeft": return GetMoveLeft();
                break;
            case "moveRight": return GetMoveRight();
                break;
            case "useItem": return GetUseItem();
                break;
            case "dropItem": return GetDropItem();
                break;
            case "pickupItem": return GetPickupItem();
                break;
            case "openInventory": return GetOpenInventory();
                break;
            case "stackModifier": return GetStackModifier();
                break;
            default: throw new ArgumentException("settingName");
        }
    }

    public static bool SetKey(string key, string settingName)
    {
        switch(settingName)
        {
            case "moveUp": return SetMoveUp(key);
                break;
            case "moveDown": return SetMoveDown(key);
                break;
            case "moveLeft": return SetMoveLeft(key);
                break;
            case "moveRight": return SetMoveRight(key);
                break;
            case "useItem": return SetUseItem(key);
                break;
            case "dropItem": return SetDropItem(key);
                break;
            case "pickupItem": return SetPickupItem(key);
                break;
            case "openInventory": return SetOpenInventory(key);
                break;
            case "stackModifier": return SetStackModifier(key);
                break;
            default: return false;
        }
    }

    #region Setters
    public static void SetDifficulty(Settings.Difficulty difficulty)
    {
        settings.difficulty = difficulty; 
    }

    public static bool SetMoveUp(string key)
    {
        if(IsValidKeyCode(key))
        {
            settings.moveUp = key;
            return true;
        }
        return false;
    }

    public static bool SetMoveDown(string key)
    {
        if(IsValidKeyCode(key))
        {
            settings.moveDown = key;
            return true;
        }
        return false;
    }

    public static bool SetMoveLeft(string key)
    {
        if(IsValidKeyCode(key))
        {
            settings.moveLeft = key;
            return true;
        }
        return false;
    }

    public static bool SetMoveRight(string key)
    {
        if(IsValidKeyCode(key))
        {
            settings.moveRight = key;
            return true;
        }
        return false;
    }

    public static bool SetDropItem(string key)
    {
        if(IsValidKeyCode(key))
        {
            settings.dropItem = key;
            return true;
        }
        return false;
    }

    public static bool SetPickupItem(string key)
    {
        if(IsValidKeyCode(key))
        {
            settings.pickupItem = key;
            return true;
        }
        return false;
    }

    public static bool SetStackModifier(string key)
    {
        if(IsValidKeyCode(key))
        {
            settings.stackModifier = key;
            return true;
        }
        return false;
    }

    public static bool SetOpenInventory(string key)
    {
        if(IsValidKeyCode(key))
        {
            settings.openInventory = key;
            return true;
        }
        return false;
    }

    public static bool SetUseItem(string key)
    {
        if(IsValidKeyCode(key))
        {
            settings.useItem = key;
            return true;
        }
        return false;
    }
    #endregion

    private static KeyCode StringToKeyCode(string key)
    {
        return (KeyCode) System.Enum.Parse(typeof(KeyCode), key, true);
    }

    private static bool IsValidKeyCode(string key)
    {
        try
        {
            StringToKeyCode(key);
            return true;
        }
        catch
        {
            return false;
        }
    }
}
