using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// Basic room object to represent a room that contains entities
/// </summary>
public class EntityRoomObj : RoomObj 
{
    EntityRoomType entityRoomType;
    int numEntities;
    int entitiesSpawned;

    public EntityRoomObj(Map map) : base(map)
    {
        entityRoomType = (EntityRoomType) gameObject.GetComponent<BaseObjectManager>().baseObject;
        numEntities = Utilities.RandomIntInRange (entityRoomType.minimumEntities, entityRoomType.maximumEntities);
        this.x = Utilities.RandomIntInRange(0, map.w);
        this.y = Utilities.RandomIntInRange(0, map.h);
        this.w = Utilities.RandomIntInRange(entityRoomType.minWidth, entityRoomType.maxWidth);
        this.h = Utilities.RandomIntInRange(entityRoomType.minHeight, entityRoomType.maxHeight);
        entitiesSpawned = 0;

        RefineSize();
    }

    override public GameObject PopulateGameObject()
    {
        // Must call parent function first!
        GameObject gameObject = base.PopulateGameObject();

        gameObject.GetComponent<BaseObjectManager>().baseObject = Resources.Load<EntityRoomType>("Rooms/Entity");

        gameObject.name = "EntityRoomObj";

        return gameObject;
    }

    /// <summary>
    /// Spawns the entities.
    /// </summary>
    protected void SpawnEntities()
    {
        List<TileObj> walkableTiles = GetWalkableTiles ();
        List<Vector2Int> spawnLocations = new List<Vector2Int> ();
        Entity[] entities = entityRoomType.entities;


        while (entitiesSpawned < numEntities) 
        {
            Debug.Log ("Number of tiles: " + tileObjs.Count);
            Debug.Log ("Walkable Tiles: " + walkableTiles.Count);
            TileObj walkableTile = walkableTiles[Utilities.RandomIntInRange (0, walkableTiles.Count)];
            Entity entityToSpawn = entities[Utilities.RandomIntInRange (0, entities.Length)];
            Vector2Int tileLocation = new Vector2Int (walkableTile.x, walkableTile.y);

            if (!spawnLocations.Contains (tileLocation)) 
            {
                // Multiply location by tile offset to account for tile spacing or tile sizes
                Vector2 locationToSpawn = new Vector2 (tileLocation.x * map.tileOffset, tileLocation.y * map.tileOffset);
                entityToSpawn.Spawn (locationToSpawn);
                spawnLocations.Add (tileLocation);
                entitiesSpawned++;
            }
        }

    }

    /// <summary>
    /// Gets the number of entities currently spawned in this room.
    /// </summary>
    /// <returns>The number of entities currently spawned in this room.</returns>
    public int GetEntitiesSpawned()
    {
        return entitiesSpawned;
    }

    public override void GenRoom()
    {
        map.FillArea(x, y, w, h, TileObj.TileObjType.FloorTileObj, this);
        //tileObjs.AddRange (addedTiles);
        SpawnEntities ();
    }

}
