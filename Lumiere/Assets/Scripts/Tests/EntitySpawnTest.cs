using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.TestTools;
using NUnit.Framework;

/// <summary>
/// Basic testing class for entity spawning.
/// </summary>
public class EntitySpawnTest 
{
    EntityRoomType entityRoomType;
    Room entityRoom;
    Entity entityToSpawn;
    Map map;
    GameObject mapGameObject;
    RoomProperties roomProperties;

    [SetUp]
    public void Init()
    {
        mapGameObject = new GameObject ("Map");
        roomProperties = Resources.Load<RoomProperties> ("RoomProperties");
        Map map = new Map (50, 50, 1, mapGameObject, roomProperties);

        entityRoomType = Resources.Load<EntityRoomType> ("Rooms/EntityRoom");
        entityToSpawn = Resources.Load<Entity> ("Entities/Monsters/Monster 1");
        entityRoom = new Room(map, 0, 0, 30, 30, entityRoomType);
        Assert.IsNotNull (map);
        Assert.IsNotNull (entityRoom);
        Assert.IsNotNull (entityRoomType);
        Assert.IsNotNull (entityToSpawn);
    }

    [TearDown]
    public void Cleanup()
    {
        GameObject[] gameObjects = GameObject.FindObjectsOfType<GameObject> ();
        foreach (GameObject gameObject in gameObjects) 
        {
            GameObject.Destroy (gameObject);
        }

        if (entityRoomType != null) 
        {
            Resources.UnloadAsset (entityRoomType);
        }

        if (entityToSpawn != null) 
        {
            Resources.UnloadAsset (entityToSpawn);
        }

        if (mapGameObject != null) 
        {
            GameObject.Destroy (mapGameObject);
        }

        map = null;
        entityRoom = null;
    }

    /// <summary>
    /// Tests whether the room tiles spawned.
    /// </summary>
    [Test]
    public void TestTilesSpawned()
    {
        entityRoom.GenRoom (5 / 2);
        Assert.IsTrue (entityRoom.tiles.Count > 0);
    }

    /// <summary>
    /// Tests whether the room exists in the scene.
    /// </summary>
    [Test]
    public void TestRoomInGame()
    {
        GameObject room = GameObject.Find ("EntityRoom");
        Assert.IsNotNull (room);
    }

    /// <summary>
    /// Tests whether the expected number of entities spawn in the game.
    /// </summary>
    [Test]
    public void TestEntitiesSpawned()
    {
        entityRoom.GenRoom (5 / 2);
        int minEntities = entityRoomType.minimumEntities;
        int maxEntities = entityRoomType.maximumEntities;
        int entitiesSpawned = entityRoomType.GetEntitiesSpawned ();
        Assert.IsTrue (entitiesSpawned >= minEntities);
        Assert.IsTrue (entitiesSpawned <= maxEntities);
    }

    /// <summary>
    /// Tests whether the entities actually exist in the scene when spawned.
    /// </summary>
    [Test]
    public void TestEntitiesInGame()
    {
        entityRoom.GenRoom (5 / 2);
        string monster1Name = "Monster 1";
        string monster2Name = "Monster 2";
        GameObject monster1 = GameObject.Find(monster1Name);
        GameObject monster2 = GameObject.Find (monster2Name);
        Assert.IsTrue (monster1 != null || monster2 != null);
    }

    /// <summary>
    /// Tests whether the entity spawns at the correct locations.
    /// </summary>
    [Test]
    public void TestEntitySpawnLocation()
    {
        Vector2 location1 = new Vector2 (0f, 0f);
        GameObject entity1 = entityToSpawn.Spawn (map, location1);
        Assert.AreEqual (location1, (Vector2) entity1.transform.position);


        Vector2 location2 = new Vector2 (-15f, -15f);
        GameObject entity2 = entityToSpawn.Spawn (map, location2);
        Assert.AreEqual (location2, (Vector2) entity2.transform.position);

        Vector2 location3 = new Vector2 (15f, 15f);
        GameObject entity3 = entityToSpawn.Spawn (map, location3);
        Assert.AreEqual (location3, (Vector2) entity3.transform.position);

        GameObject.Destroy (entity1);
        GameObject.Destroy (entity2);
        GameObject.Destroy (entity3);
    }
}
