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
    Room secretRoom;
    Map map;
    GameObject mapGameObject;
    RoomProperties roomProperties;

    [SetUp]
    public void Init()
    {
        mapGameObject = new GameObject ("Map");
        roomProperties = Resources.Load<RoomProperties> ("RoomProperties");
        Map map = new Map (50, 50, 1, mapGameObject, roomProperties);
        secretRoomType = Resources.Load<SecretRoomType> ("Rooms/SecretRoom");
        secretRoom = new Room (map, 0, 0, 30, 30, secretRoomType);
        Assert.IsNotNull (map);
        Assert.IsNotNull (secretRoom);
        Assert.IsNotNull (secretRoomType);
    }

    [TearDown]
    public void Cleanup()
    {
        GameObject[] gameObjects = GameObject.FindObjectsOfType<GameObject> ();
        foreach (GameObject gameObject in gameObjects) 
        {
            GameObject.Destroy (gameObject);
        }

        if (secretRoomType != null) 
        {
            Resources.UnloadAsset (secretRoomType);
        }

        if (mapGameObject != null) 
        {
            GameObject.Destroy (mapGameObject);
        }

        map = null;
        secretRoom = null;
    }

    /// <summary>
    /// Tests whether the room tiles spawned.
    /// </summary>
    [Test]
    public void TestTilesSpawned()
    {
        secretRoom.GenRoom (5 / 2);
        Assert.IsTrue (secretRoom.tiles.Count > 0);
    }

    /// <summary>
    /// Tests whether the room exists in the scene.
    /// </summary>
    [Test]
    public void TestRoomInGame()
    {
        GameObject room = GameObject.Find ("SecretRoom");
        Assert.IsNotNull (room);
    }

}