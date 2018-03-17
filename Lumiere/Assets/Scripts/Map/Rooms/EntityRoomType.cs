using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Room/Entity")]
public class EntityRoomType : RoomType 
{
    public Entity[] entities;
    public int minimumEntities;
    public int maximumEntities;
    public TileType floorTile;
    private int entitiesSpawned;

    /// <summary>
    /// Spawns the entities.
    /// </summary>
    protected void SpawnEntities(Room room, Map map)
    {
        List<Tile> walkableTiles = room.GetWalkableTiles ();
        List<Vector2Int> spawnLocations = new List<Vector2Int> ();
        int numEntities = Utilities.RandomIntInRange (minimumEntities, maximumEntities);
        entitiesSpawned = 0;

        while (entitiesSpawned < numEntities) 
        {
            //Debug.Log ("Number of tiles: " + tileObjs.Count);
            //Debug.Log ("Walkable Tiles: " + walkableTiles.Count);
            Tile walkableTile = walkableTiles[Utilities.RandomIntInRange (0, walkableTiles.Count)];
            Entity entityToSpawn = entities[Utilities.RandomIntInRange (0, entities.Length)];
            Vector2Int tileLocation = new Vector2Int (walkableTile.x, walkableTile.y);

            if (!spawnLocations.Contains (tileLocation)) 
            {
                // Multiply location by tile offset to account for tile spacing or tile sizes
                Vector2 locationToSpawn = new Vector2 (tileLocation.x * map.tileOffset, tileLocation.y * map.tileOffset);
                entityToSpawn.Spawn (map, locationToSpawn);
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

    public override void GenRoom(Room room, Map map)
    {
        int x = room.x;
        int y = room.y;
        int w = room.w;
        int h = room.h;
        map.FillArea(x, y, w, h, floorTile, room);
        SpawnEntities (room, map);
    }
}
