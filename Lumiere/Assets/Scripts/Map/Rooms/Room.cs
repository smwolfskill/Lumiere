﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Room : Container
{
    public int x, y;
    public int w, h;

    public List<Door> doors;

    public RoomType roomType;

    public Room(Map map, int x, int y, int w, int h, RoomType roomType) : base(map, roomType)
    {
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;

        this.doors = new List<Door>();

        this.roomType = (RoomType)this.containerType;
    }

    public void GenRoom()
    {
        roomType.GenRoom (this, map);

        return;

        int doorAttempts = Utilities.RandomIntInRange(1, 5);
        // Add some doors
        for (int doorAttempt = 0; doorAttempt < doorAttempts;  doorAttempt++)
        {
            int incrementOffOfWidth = Utilities.RandomIntInRange(1, w);
            int incrementOffOfHeight = Utilities.RandomIntInRange(1, h);
            Door door;
            switch (Utilities.RandomEnumValue<Utilities.Direction>())
            {
                case Utilities.Direction.NORTH:
                    door = new Door(x + incrementOffOfWidth, y);
                    break;
                case Utilities.Direction.SOUTH:
                    door = new Door(x + incrementOffOfWidth, y + h - 1);
                    break;
                case Utilities.Direction.WEST:
                    door = new Door(x, y + incrementOffOfHeight);
                    break;
                case Utilities.Direction.EAST:
                    door = new Door(x + w - 1, y + incrementOffOfHeight);
                    break;
                default:
                    door = null;
                    break;
            }

            if(IsDoorValid(door))
            {
                doors.Add(door);
            }
        }

    }

    private bool IsDoorValid(Door doorAttempt)
    {
        foreach(Door door in doors)
        {
            if (doorAttempt.x == door.x && doorAttempt.y == door.y)
                return false;
        }

        return true;
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
