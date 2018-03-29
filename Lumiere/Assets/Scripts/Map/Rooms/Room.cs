using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Room : Container
{
    public int x, y;
    public int w, h;



    public RoomType roomType;

    public Room(Map map, int x, int y, int w, int h, RoomType roomType) : base(map, roomType)
    {
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;

        this.roomType = (RoomType)this.containerType;
    }

    public void GenRoom()
    {
        roomType.GenRoom (this, map);
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
