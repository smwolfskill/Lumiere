using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.IO;
using System;

public static class SettingsManager
{
    public static volatile bool loaded = false;
    public const string DEFAULT_PATH = "Assets/Resources/settings.txt";
    private static Settings settings;

    /// <summary>
    /// Loads the settings from a given file, if there is a problem loading
    /// the file the defaults are loaded from the Settings class.
    /// </summary>
    /// <param name="file_path">The string containing the path to the file holding settings</param>
    /// <returns>Returns true if loading from file was successful.</returns>
    public static bool LoadSettings(string file_path = DEFAULT_PATH) //TODO: main game loop load settings upon game launch
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
            //Problem with loading from file, so set default settings.
            settings = new Settings();
            return false;
        }
    }

    /// <summary>
    /// Saves the settings to a given file.
    /// </summary>
    /// <param name="file_path">The string containing the path to the file that 
    /// settings should be written to.</param>
    /// <returns>Returns true if writing to the file was successful.</returns>
    public static bool SaveSettings(string file_path = DEFAULT_PATH)
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

    /// <summary>
    /// Getter for the corresponding setting. Generally only used by the GetKey function
    /// in this class.
    /// </summary>
    /// <returns>Returns a Unity KeyCode stored for the corresponding setting.</returns>
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

    public static KeyCode GetWalk()
    {
        return StringToKeyCode(settings.walk);
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

    public static KeyCode GetOpenMenu()
    {
        return StringToKeyCode(settings.openMenu);
    }
    #endregion
    
    /// <summary>
    /// Calls the getter for the corresponding setting.
    /// </summary>
    /// <param name="settingName">The name of which setting to return.</param>
    /// <returns>Returns KeyCode stored for the corresponding setting.
    /// Throws an ArgumentException if the passed settingName param is invalid.</returns>
    public static KeyCode GetKey(string settingName)
    {
        switch(settingName)
        {
            case "moveUp": return GetMoveUp();
            case "moveDown": return GetMoveDown();
            case "moveLeft": return GetMoveLeft();
            case "moveRight": return GetMoveRight();
            case "walk": return GetWalk();
            case "useItem": return GetUseItem();
            case "dropItem": return GetDropItem();
            case "pickupItem": return GetPickupItem();
            case "openInventory": return GetOpenInventory();
            case "stackModifier": return GetStackModifier();
            case "openMenu": return GetOpenMenu();
            default: throw new ArgumentException("settingName");
        }
    }

    /// <summary>
    /// Calls the setter for the corresponding setting.
    /// </summary>
    /// <param name="settingName">The name of which setting to set.</param>
    /// <returns>Returns true if it successfully set the corresponding setting.
    /// Returns false otherwise.</returns>
    public static bool SetKey(string key, string settingName)
    {
        switch(settingName)
        {
            case "moveUp": return SetMoveUp(key);
            case "moveDown": return SetMoveDown(key);
            case "moveLeft": return SetMoveLeft(key);
            case "moveRight": return SetMoveRight(key);
            case "walk": return SetWalk(key);
            case "useItem": return SetUseItem(key);
            case "dropItem": return SetDropItem(key);
            case "pickupItem": return SetPickupItem(key);
            case "openInventory": return SetOpenInventory(key);
            case "stackModifier": return SetStackModifier(key);
            case "openMenu": return SetOpenMenu(key);
            default: return false;
        }
    }

    
    #region Setters
    public static void SetDifficulty(Settings.Difficulty difficulty)
    {
        settings.difficulty = difficulty; 
    }

    /// <summary>
    /// Setter for the corresponding setting. Generally only used by the SetKey function
    /// in this class.
    /// </summary>
    /// <param name="key">The Unity KeyCode to set.</param>
    /// <returns>Returns true if the setting was set successfully.</returns>
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

    public static bool SetWalk(string key)
    {
        if(IsValidKeyCode(key))
        {
            settings.walk = key;
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

    public static bool SetOpenMenu(string key)
    {
        if(IsValidKeyCode(key))
        {
            settings.openMenu = key;
            return true;
        }
        return false;
    }
    #endregion

    /// <summary>
    /// Converts a given string to it's corresponding Unity KeyCode
    /// </summary>
    /// <param name="key">The string to convert.</param>
    /// <returns>Returns the KeyCode that correspondes to the passed key.</returns>
    private static KeyCode StringToKeyCode(string key)
    {
        return (KeyCode) System.Enum.Parse(typeof(KeyCode), key, true);
    }

    /// <summary>
    /// Checks if a given string corresponds to a KeyCode
    /// </summary>
    /// <param name="key">The string to check.</param>
    /// <returns>Returns true if the string corresponds to a KeyCode, false otherwise.</returns>
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
