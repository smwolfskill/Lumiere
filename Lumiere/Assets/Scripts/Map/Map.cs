using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Map
{

    private Tile[,] tileMatrix;
    private List<Room> rooms;
    private RoomProperties roomProperties;

    public int w, h;
    public int tileOffset;
    public GameObject gameObject;

    public Map(int w, int h, int tileOffset, GameObject gameObject, RoomProperties roomProperties)
    {
        this.gameObject = gameObject;
        this.w = w;
        this.h = h;
        this.tileOffset = tileOffset;
        this.tileMatrix = new Tile[h, w];
        this.rooms = new List<Room>();
        this.roomProperties = roomProperties;
    }

    /// TODO: this description was from legacy code, change it to fit this (somewhat
    ///       similar) code
    ///  
    /// <summary>     
    /// Applies a GameObject to a coordinate in the tileMap as well as adding that tile
    /// to the Unity heirarchy as a child to a Container. Removes the tile that existed 
    /// in the specified location if a tile exists.
    /// </summary>
    /// <returns>
    /// True if the tile was placed correctly, false otherwise (such as when the tile
    /// cannot be placed in the specified coordinate due to the coordinate being invalid)
    /// </returns>
    public Tile SetTile(int x, int y, Tile tile, Room room)
    {
        if (!ValidTileSpace(x, y))
            return null;

        Tile preExistingTile = GetTile(x, y);
        if (preExistingTile != null)
            preExistingTile.Remove();

        this.tileMatrix[y, x] = tile;

        tile.SetX_Y(x, y);
        tile.SetRoom(room);
        tile.SetMap(this);

        room.AddTile(tile);

        return tile;
    }

    public void FillArea(int x, int y, int w, int h, TileType tileType, Room room)
    {
        // Force top and left of rectangle to be inside the map.
        if (x < 0)
            x = 0;
        if (y < 0)
            y = 0;

        // Force width and height of rectangle to not be outside the map.
        if (x + w > this.w)
            w = this.w - x;
        if (y + h > this.h)
            h = this.h - y;

        for (int currX = x; currX < x + w; currX++)
        {
            for (int currY = y; currY < y + h; currY++)
            {
                Tile tileToAdd = 
                SetTile(
                    currX, currY,
                        new Tile(x, y, this, tileType),
                    room
                );
            }
        }
    }

    public void FillLine(int x, int y, int length, Utilities.Direction direction, TileType tileType, Room room)
    {
        switch (direction)
        {
            case Utilities.Direction.NORTH:
                FillArea(x, y - length, 1, length, tileType, room);
                break;

            case Utilities.Direction.SOUTH:
                FillArea(x, y, 1, length, tileType, room);
                break;

            case Utilities.Direction.WEST:
                FillArea(x - length, y, length, 1, tileType, room);
                break;

            case Utilities.Direction.EAST:
                FillArea(x, y, length, 1, tileType, room);
                break;
        }
    }

    public Tile GetTile(int x, int y)
    {
        if (!ValidTileSpace(x, y))
            return null;

        return tileMatrix[y, x];
    }

    public Room GenRoom(RoomType roomType)
    {
        int x = Utilities.RandomIntInRange (0, w);
        int y = Utilities.RandomIntInRange (0, h);
        int minWidth = roomProperties.minWidth;
        int maxWidth = roomProperties.maxWidth;
        int minHeight = roomProperties.minHeight;
        int maxHeight = roomProperties.maxHeight;
        int roomWidth = Utilities.RandomIntInRange (minWidth, maxWidth);
        int roomHeight = Utilities.RandomIntInRange (minHeight, maxHeight);
        return new Room (this, x, y, roomWidth, roomHeight, roomType);
    }

    public Room GenRandomRoom()
    {
        Room room;
        do
        {
            RoomType[] roomTypes = roomProperties.roomTypes;
            RoomType roomType = roomTypes[Utilities.RandomIntInRange(0, roomTypes.Length)];
            room = GenRoom(roomType);
        }
        while (room == null);

        return room;
    }

    public void AddRoom(Room room)
    {
        this.rooms.Add(room);
        room.gameObject.transform.parent = this.gameObject.transform;
    }

    public bool IsRoomAreaValid(Room room, TileType[] avoidTiles)
    {
        for (int currX = room.x; currX < room.x + room.w; currX++)
        {
            for (int currY = room.y; currY < room.y + room.h; currY++)
            {
                Tile currTileObj = GetTile(currX, currY);

                if (currTileObj != null)
                {
                    foreach (TileType avoidTile in avoidTiles)
                    {
                        if (avoidTile == currTileObj.tileType)
                            return false;
                    }
                }
            }
        }

        return true;
    }

    /// TODO: this description was from legacy code, change it to fit this (somewhat
    ///       similar) code
    ///  
    /// <summary>
    /// Checks if a coordinate is one that exists within the boundaries of the map/
    /// </summary>
    /// TODO: this belongs in a seperate script as it is not directly tied to the
    ///       concept of GeneratingTiles
    private bool ValidTileSpace(int x, int y)
    {
        return (x >= 0 && y >= 0 && x < h && y < h);
    }

    public Room GetRanRoom(RoomType[] ignoreRoomObjTypes)
    {
        Room room;
        bool isInIgnoreRoomObjTypes = false;
        do
        {
            isInIgnoreRoomObjTypes = false;
            room = this.rooms[Utilities.RandomIntInRange(0, rooms.Count)];

            foreach (RoomType roomType in ignoreRoomObjTypes)
            {
                if (roomType == room.roomType) isInIgnoreRoomObjTypes = true;
            }
        }
        while (isInIgnoreRoomObjTypes);

        return room;
    }

    public void RemoveRoom(Room room)
    {
        this.rooms.Remove(room);
    }

}
