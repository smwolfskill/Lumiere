using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.TestTools;
using NUnit.Framework;

/// <summary>
/// Basic testing class for hideout room generation.
/// </summary>
public class HideoutRoomTest 
{
    HideoutRoomType hideoutRoomType;
    HideoutRoomObj hideoutRoomObj;
    Map map;
    GameObject mapGameObject;

    [SetUp]
    public void Init()
    {
        mapGameObject = new GameObject ("Map");
        Map map = new Map (50, 50, 1, mapGameObject);
        hideoutRoomObj = new HideoutRoomObj (map);
        hideoutRoomType = Resources.Load<HideoutRoomType> ("Rooms/Hideout");
        Assert.IsNotNull (map);
        Assert.IsNotNull (hideoutRoomObj);
        Assert.IsNotNull (hideoutRoomType);
    }

    [TearDown]
    public void Cleanup()
    {
        if (hideoutRoomType != null) 
        {
            Resources.UnloadAsset (hideoutRoomType);
        }

        if (mapGameObject != null) 
        {
            GameObject.Destroy (mapGameObject);
        }

        map = null;
        hideoutRoomObj = null;
    }

    /// <summary>
    /// Tests whether the room tiles spawned.
    /// </summary>
    [Test]
    public void TestTilesSpawned()
    {
        hideoutRoomObj.GenRoom ();
        Assert.IsTrue (hideoutRoomObj.tileObjs.Count > 0);
    }

    /// <summary>
    /// Tests whether the room exists in the scene.
    /// </summary>
    [Test]
    public void TestRoomInGame()
    {
        GameObject room = GameObject.Find ("HideoutRoomObj");
        Assert.IsNotNull (room);
    }

}
