using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Room : Container
{
    public int x, y;
    public int w, h;
    public int centerX, centerY;

    // A list of all other rooms, sorted by closest to furthest based
    // on centerX, centerY
    public List<Room> closestOtherRooms;

    public List<Door> doors;

    public RoomType roomType;

    public Room(Map map, int x, int y, int w, int h, RoomType roomType) : base(map, roomType)
    {
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;

        this.centerX = x + w / 2;
        this.centerY = y + h / 2;

        this.doors = new List<Door>();
        this.closestOtherRooms = new List<Room>();

        this.roomType = (RoomType)this.containerType;
    }

    public void GenRoom(int doorRadius)
    {
        roomType.GenRoom(this, map);

        GenDoors(doorRadius);
    }

    private void GenDoors(int radius)
    {
        if (radius > w - radius || radius > h - radius) return;

        int doorAttempts = Utilities.RandomIntInRange(10, 50);
        // Add some doors
        for (int doorAttempt = 0; doorAttempt < doorAttempts; doorAttempt++)
        {
            int incrementOffOfWidth = Utilities.RandomIntInRange(radius, w - radius);
            int incrementOffOfHeight = Utilities.RandomIntInRange(radius, h - radius);
            Door door;
            switch (Utilities.RandomEnumValue<Utilities.Direction>())
            {
                case Utilities.Direction.NORTH:
                    door = new Door(x + incrementOffOfWidth, y, Utilities.Direction.NORTH, this, radius);
                    break;
                case Utilities.Direction.SOUTH:
                    door = new Door(x + incrementOffOfWidth, y + h - 1, Utilities.Direction.SOUTH, this, radius);
                    break;
                case Utilities.Direction.WEST:
                    door = new Door(x, y + incrementOffOfHeight, Utilities.Direction.WEST, this, radius);
                    break;
                case Utilities.Direction.EAST:
                    door = new Door(x + w - 1, y + incrementOffOfHeight, Utilities.Direction.EAST, this, radius);
                    break;
                default:
                    door = null;
                    break;
            }

            if (IsDoorValid(door))
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
