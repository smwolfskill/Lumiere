using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Map
{

    private TileObj[,] tileObjMatrix;
    private List<RoomObj> rooms;

    public int w, h;
    public int tileOffset;
    public GameObject gameObject;

    public Map(int w, int h, int tileOffset, GameObject gameObject)
    {
        this.gameObject = gameObject;
        this.w = w;
        this.h = h;
        this.tileOffset = tileOffset;
        this.tileObjMatrix = new TileObj[h, w];
        this.rooms = new List<RoomObj>();
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
    public TileObj SetTile(int x, int y, TileObj tileObj, RoomObj roomObj)
    {
        if (!ValidTileSpace(x, y))
            return null;

        TileObj preExistingTile = GetTile(x, y);
        if (preExistingTile != null)
            preExistingTile.Remove();

        this.tileObjMatrix[y, x] = tileObj;

        tileObj.SetX_Y(x, y);
        tileObj.SetRoom(roomObj);
        tileObj.SetMap(this);

        roomObj.AddTile(tileObj);

        return tileObj;
    }

    public List<TileObj> FillArea(int x, int y, int w, int h, TileObj.TileObjType tileObjType, RoomObj roomObj)
    {
        List<TileObj> addedTiles = new List<TileObj> ();

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
                TileObj tileToAdd = TileObj.InstantiateTileObj (currX, currY, this, tileObjType);
                SetTile(
                    currX, currY,
                    tileToAdd,
                    roomObj
                );

                addedTiles.Add (tileToAdd);
            }
        }

        return addedTiles;
    }

    public TileObj GetTile(int x, int y)
    {
        if (!ValidTileSpace(x, y))
            return null;

        return tileObjMatrix[y, x];
    }

    public RoomObj GenRoom(RoomObj.RoomObjType roomObjType)
    {
        return RoomObj.InstantiateRoomObj(roomObjType, this);
    }

    public RoomObj GenRandomRoom()
    {
        RoomObj roomObj;
        do
        {
            RoomObj.RoomObjType roomObjType = Utilities.RandomEnumValue<RoomObj.RoomObjType>();
            roomObj = GenRoom(roomObjType);
        }
        while (roomObj == null);

        return roomObj;
    }

    public void AddRoom(RoomObj roomObj)
    {
        this.rooms.Add(roomObj);
    }

    public bool IsRoomAreaValid(RoomObj roomObj, TileObj.TileObjType[] avoidTiles)
    {
        for (int currX = roomObj.x; currX < roomObj.x + roomObj.h; currX++)
        {
            for (int currY = roomObj.y; currY < roomObj.y + roomObj.h; currY++)
            {
                TileObj currTileObj = GetTile(currX, currY);

                if (currTileObj != null)
                {
                    foreach (TileObj.TileObjType avoidTile in avoidTiles)
                    {
                        if (avoidTile == currTileObj.tileObjType)
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

}
