using UnityEngine;
using System;
using NUnit.Framework;
using NUnit.Framework.Internal;

/// <summary>
/// Tests the SettingsManagerClass.
/// </summary>
public class SettingsManagerTest
{
    string path = "Assets/Resources/settings.txt";

    /// <summary>
    /// Setup phase for initilizing tests.
    /// </summary>
    [SetUp]
    public void Init()
    {

    }

    /// <summary>
    /// Save defaults back to file
    /// </summary>
    [TearDown]
    public void TearDown()
    {
        if (SettingsManager.loaded == true)
        {
            SettingsManager.LoadSettings("Defaults");
            SettingsManager.SaveSettings(path);    
        }
    }

    /// <summary>
    /// Does the item datatype store its data correctly, and can it be retrieved and set?
    /// </summary>
    [Test]
    public void LoadSettingsInitial()
    {
        SettingsManager.loaded = false;
        if(!SettingsManager.loaded)
        {
            SettingsManager.LoadSettings("Defaults"); 
        }
        Assert.AreEqual(true, SettingsManager.loaded);
    }

    /// <summary>
    /// Checks the GetKey method against the default settings
    /// </summary>
    [Test]
    [TestCase(KeyCode.W, "moveUp")]
    [TestCase(KeyCode.A, "moveLeft")]
    [TestCase(KeyCode.D, "moveRight")]
    [TestCase(KeyCode.S, "moveDown")]
    [TestCase(KeyCode.E, "useItem")]
    [TestCase(KeyCode.Q, "dropItem")]
    [TestCase(KeyCode.Mouse0, "pickupItem")]
    [TestCase(KeyCode.LeftShift, "stackModifier")]
    [TestCase(KeyCode.Tab, "openInventory")]
    [TestCase(KeyCode.Tab, "FakeSetting")]
    public void TestGetKey(KeyCode expected, string settingName)
    {
        SettingsManager.LoadSettings("Defaults"); 
        
        try
        {
            KeyCode value = SettingsManager.GetKey(settingName);
            Assert.AreEqual(expected, value);
        }
        catch (ArgumentException e)
        {
            Assert.AreEqual("FakeSetting", settingName);
        }
    }

    /// <summary>
    /// Checks the SetKey method against a variety of inputs
    /// </summary>
    [Test]
    [TestCase("W", "moveUp")]
    [TestCase("A", "moveLeft")]
    [TestCase("D", "moveRight")]
    [TestCase("S", "moveDown")]
    [TestCase("E", "useItem")]
    [TestCase("Q", "dropItem")]
    [TestCase("Mouse0", "pickupItem")]
    [TestCase("LeftShift", "stackModifier")]
    [TestCase("Tab", "openInventory")]
    [TestCase("Tab", "FakeSetting")]
    public void TestSetKey(string new_value, string settingName)
    {
        SettingsManager.LoadSettings("Defaults");
        if(settingName == "FakeSetting")
        {
            Assert.AreEqual(false, SettingsManager.SetKey(new_value, settingName));
            return;
        }
        Assert.AreEqual(true, SettingsManager.SetKey(new_value, settingName));
        KeyCode expected = (KeyCode) System.Enum.Parse(typeof(KeyCode), new_value);
        Assert.AreEqual(expected, SettingsManager.GetKey(settingName));
    }

    /// <summary>
    /// Test getting the default difficulty
    /// </summary>
    public void TestGetDefaultDifficulty()
    {
        SettingsManager.LoadSettings("Defaults"); 
        Assert.AreEqual(Settings.Difficulty.Normal, SettingsManager.GetDifficulty());
    }

    /// <summary>
    /// Checks the SetDifficulty method against each difficulty level
    /// </summary>
    [Test]
    [TestCase(Settings.Difficulty.Easy)]
    [TestCase(Settings.Difficulty.Normal)]
    [TestCase(Settings.Difficulty.Hard)]
    public void TestSetDifficulty(Settings.Difficulty new_value)
    {
        SettingsManager.LoadSettings("Defaults"); 
        SettingsManager.SetDifficulty(new_value);
        Assert.AreEqual(new_value, SettingsManager.GetDifficulty());
    }

    /// <summary>
    /// Checks the SaveSettings method
    /// </summary>
    [Test]
    [TestCase("W", "moveUp")]
    [TestCase("A", "moveLeft")]
    [TestCase("D", "moveRight")]
    [TestCase("S", "moveDown")]
    [TestCase("E", "useItem")]
    [TestCase("Q", "dropItem")]
    [TestCase("Mouse0", "pickupItem")]
    [TestCase("LeftShift", "stackModifier")]
    [TestCase("U", "openInventory")]
    public void TestSaveSettings(string new_value, string settingName)
    {
        SettingsManager.LoadSettings("Defaults");
        Assert.AreEqual(true, SettingsManager.SetKey(new_value, settingName));
        SettingsManager.SaveSettings(path);
        SettingsManager.LoadSettings(path);
        KeyCode expected = (KeyCode) System.Enum.Parse(typeof(KeyCode), new_value);
        Assert.AreEqual(expected, SettingsManager.GetKey(settingName));
    }
}