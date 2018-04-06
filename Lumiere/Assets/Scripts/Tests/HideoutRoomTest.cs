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
    Room hideoutRoom;
    Map map;
    GameObject mapGameObject;
    RoomProperties roomProperties;

    [SetUp]
    public void Init()
    {
        mapGameObject = new GameObject ("Map");
        roomProperties = Resources.Load<RoomProperties> ("RoomProperties");
        Map map = new Map (50, 50, 1, mapGameObject, roomProperties);
        hideoutRoomType = Resources.Load<HideoutRoomType> ("Rooms/HideoutRoom");
        hideoutRoom = new Room (map, 0, 0, 30, 30, hideoutRoomType);
        Assert.IsNotNull (map);
        Assert.IsNotNull (hideoutRoom);
        Assert.IsNotNull (hideoutRoomType);
    }

    [TearDown]
    public void Cleanup()
    {
        GameObject[] gameObjects = GameObject.FindObjectsOfType<GameObject> ();
        foreach (GameObject gameObject in gameObjects) 
        {
            GameObject.Destroy (gameObject);
        }

        if (hideoutRoomType != null) 
        {
            Resources.UnloadAsset (hideoutRoomType);
        }

        if (mapGameObject != null) 
        {
            GameObject.Destroy (mapGameObject);
        }

        map = null;
        hideoutRoom = null;
    }

    /// <summary>
    /// Tests whether the room tiles spawned.
    /// </summary>
    [Test]
    public void TestTilesSpawned()
    {
        hideoutRoom.GenRoom (5 / 2);
        Assert.IsTrue (hideoutRoom.tiles.Count > 0);
    }

    /// <summary>
    /// Tests whether the room exists in the scene.
    /// </summary>
    [Test]
    public void TestRoomInGame()
    {
        GameObject room = GameObject.Find ("HideoutRoom");
        Assert.IsNotNull (room);
    }

}
