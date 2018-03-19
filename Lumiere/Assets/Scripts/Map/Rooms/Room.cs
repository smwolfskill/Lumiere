using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Room
{
    public int x, y;
    public int w, h;

    public GameObject gameObject;
    protected Map map;
    public RoomType roomType;
    public List<Tile> tiles;

    public Room(Map map, int x, int y, int w, int h, RoomType roomType)
    {
        this.map = map;
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;
        this.roomType = roomType;
        tiles = new List<Tile> ();

        gameObject = roomType.PopulateGameObject ();
    }

    public void GenRoom()
    {
        roomType.GenRoom (this, map);
    }

    public void AddTile(Tile tile)
    {
        tile.gameObject.transform.parent = gameObject.transform;

        tiles.Add(tile);
    }

    public void RemoveTile(Tile tile)
    {
        tiles.Remove(tile);
    }
        
    public List<Tile> GetWalkableTiles()
    {
        List<Tile> walkableTiles = new List<Tile> ();
        foreach (Tile tile in tiles) 
        {
            if (tile.IsWalkable ()) 
            {
                walkableTiles.Add (tile);
            }
        }

        return walkableTiles;
    }

    public void Remove()
    {
        map.RemoveRoom(this);
        Object.Destroy(gameObject);
    }

    public void RefineSize()
    {
        if (w + x > map.w) w = map.w - x;
        if (h + y > map.h) h = map.h - y;
    }

    public virtual PlayerObject SpawnPlayer()
    {
        //find location, spawn player
        List<Tile> walkableTiles = GetWalkableTiles();
        Tile walkableTile = walkableTiles[Utilities.RandomIntInRange(0, walkableTiles.Count)];
        Vector2Int tileLocation = new Vector2Int(walkableTile.x, walkableTile.y);
        Player playerToSpawn = roomType.player;

        // Multiply location by tile offset to account for tile spacing or tile sizes
        Vector2 locationToSpawn = new Vector2(tileLocation.x * map.tileOffset, tileLocation.y * map.tileOffset);
        GameObject playerGameObject = playerToSpawn.Spawn(map, locationToSpawn);

        return (PlayerObject)playerGameObject.GetComponent<EntityObjectManager>().entityObject;
    }

}
