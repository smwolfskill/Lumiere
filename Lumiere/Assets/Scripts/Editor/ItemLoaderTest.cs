using UnityEngine;
using UnityEditor;
using UnityEngine.TestTools;
using NUnit.Framework;
using System.Collections;
using UnityEditor.VersionControl;
using NUnit.Framework.Internal;
using System.IO;

public class ItemLoaderTest 
{
    Sprite testSprite;
    GameItem testItem;
    string saveAndLoadFile = "SaveAndLoadTest.json";

    /// <summary>
    /// Run only once before tests run
    /// </summary>
    [SetUp]
    public void Init()
    {
        Entity entity = Resources.Load<Entity>("GamePlayer");
        testSprite = entity.GetSprite ();
        Assert.IsNotNull (testSprite);
        testItem = new GameItem (testSprite, null, "testName", "testDesc", 2.53, GameItem.ItemRarity.LEGENDARY, 4, 30, 22);
    }

    /// <summary>
    /// Called after all tests finish. Remove testing file.
    /// </summary>
    [OneTimeTearDown]
    public void Cleanup() 
    {
        if (File.Exists (saveAndLoadFile))
        {
            File.Delete (saveAndLoadFile);
        }
    }

    [Test]
    public void SaveAndLoadTest()
    {
        bool saveSuccess = ItemLoader.SaveItem (testItem, saveAndLoadFile);
        Assert.AreEqual (true, saveSuccess);
        GameItem loadedItem = ItemLoader.LoadItem(saveAndLoadFile);
        Assert.AreEqual (true, testItem.Equals (loadedItem));
    }

}
