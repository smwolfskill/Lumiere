using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.TestTools;
using NUnit.Framework;

/// <summary>
/// Basic testing class for secret room generation.
/// </summary>
public class SecretRoomTest 
{
    SecretRoomType secretRoomType;
    SecretRoomObj secretRoomObj;
    Map map;
    GameObject mapGameObject;

    [SetUp]
    public void Init()
    {
        mapGameObject = new GameObject ("Map");
        Map map = new Map (50, 50, 1, mapGameObject);
        secretRoomObj = new SecretRoomObj (map);
        secretRoomType = Resources.Load<SecretRoomType> ("Rooms/Secret");
        Assert.IsNotNull (map);
        Assert.IsNotNull (secretRoomObj);
        Assert.IsNotNull (secretRoomType);
    }

    [TearDown]
    public void Cleanup()
    {
        if (secretRoomType != null) 
        {
            Resources.UnloadAsset (secretRoomType);
        }

        if (mapGameObject != null) 
        {
            GameObject.Destroy (mapGameObject);
        }

        map = null;
        secretRoomObj = null;
    }

    /// <summary>
    /// Tests whether the room tiles spawned.
    /// </summary>
    [Test]
    public void TestTilesSpawned()
    {
        secretRoomObj.GenRoom ();
        Assert.IsTrue (secretRoomObj.tileObjs.Count > 0);
    }

    /// <summary>
    /// Tests whether the room exists in the scene.
    /// </summary>
    [Test]
    public void TestRoomInGame()
    {
        GameObject room = GameObject.Find ("SecretRoomObj");
        Assert.IsNotNull (room);
    }

}