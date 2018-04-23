using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Room/Entity")]
public class EntityRoomType : RoomType 
{
    public Entity[] entities;
    public float minimumEntitiesRatio;
    public float maximumEntitiesRatio;
    public TileType floorTile;
    public TileType wallTile;
    private int entitiesSpawned;

    /// <summary>
    /// Spawns the entities.
    /// </summary>
    protected int SpawnEntities(Room room, Map map)
    {

        int minimunEntities = GetMinEntities(map);
        int maximumEntities = GetMaxEntities(map);

        int numEntities = Utilities.RandomIntInRange (minimunEntities, maximumEntities);

        return SpawnEntities(numEntities, room, map);
    }

    public int SpawnEntities(int numEntities, Room room, Map map)
    {
        List<Tile> walkableTiles = room.GetWalkableTiles();
        List<Vector2Int> spawnLocations = new List<Vector2Int>();

        entitiesSpawned = 0;

        int numAttempts = 100;
        int currAttempt = 0;

        while (entitiesSpawned < numEntities && currAttempt < numAttempts)
        {
            //Debug.Log ("Number of tiles: " + tileObjs.Count);
            //Debug.Log ("Walkable Tiles: " + walkableTiles.Count);
            Tile walkableTile = walkableTiles[Utilities.RandomIntInRange(0, walkableTiles.Count)];
            Entity entityToSpawn = entities[Utilities.RandomIntInRange(0, entities.Length)];
            Vector2Int tileLocation = new Vector2Int(walkableTile.x, walkableTile.y);

            if (!spawnLocations.Contains(tileLocation))
            {
                // Multiply location by tile offset to account for tile spacing or tile sizes
                Vector2 locationToSpawn = new Vector2(tileLocation.x * map.tileOffset, tileLocation.y * map.tileOffset);
                entityToSpawn.Spawn(map, locationToSpawn);
                spawnLocations.Add(tileLocation);
                entitiesSpawned++;
            }

            currAttempt++;
        }

        return entitiesSpawned;
    }

    public int GetMinEntities(Map map)
    {
        return (int)(minimumEntitiesRatio * map.difficulty * map.levelNumber);
    }
    public int GetMaxEntities(Map map)
    {
        return (int)(maximumEntitiesRatio * map.difficulty * map.levelNumber);
    }

    /// <summary>
    /// Gets the number of entities currently spawned in this room.
    /// </summary>
    /// <returns>The number of entities currently spawned in this room.</returns>
    public int GetEntitiesSpawned()
    {
        return entitiesSpawned;
    }

    public override void GenRoom(Room room, Map map)
    {
        map.FillAreaWithBorder(room.x, room.y, room.w, room.h, floorTile, wallTile, room);
        SpawnEntities(room, map);

    }
}
