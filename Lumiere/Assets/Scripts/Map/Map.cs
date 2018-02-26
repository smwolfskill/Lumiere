using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Map
{

    private TileObj[,] tileObjMatrix;
    private List<RoomObj> rooms;

    public Vector2Int w_h;
    public int tileOffset;
    public GameObject gameObject;

    public Map(Vector2Int w_h, int tileOffset, GameObject gameObject)
    {
        this.gameObject = gameObject;
        this.w_h = w_h;
        this.tileOffset = tileOffset;
        this.tileObjMatrix = new TileObj[w_h.y, w_h.x];
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
    public TileObj SetTile(Vector2Int x_y, TileObj tileObj, RoomObj roomObj)
    {
        if (!ValidTileSpace(x_y))
            return null;

        TileObj preExistingTile = GetTile(x_y);
        if (preExistingTile != null)
            preExistingTile.Remove();

        this.tileObjMatrix[x_y.y, x_y.x] = tileObj;

        tileObj.SetX_Y(x_y);
        tileObj.SetRoom(roomObj);
        tileObj.SetMap(this);

        roomObj.AddTile(tileObj);

        return tileObj;
    }

    public void FillArea(Vector2Int x_y, Vector2Int w_h, TileObj.TileObjType tileObjType, RoomObj roomObj)
    {
        // Force top and left of rectangle to be inside the map.
        if (x_y.x < 0)
            x_y.x = 0;
        if (x_y.y < 0)
            x_y.y = 0;

        // Force width and height of rectangle to not be outside the map.
        if (x_y.x + w_h.x > this.w_h.x)
            w_h.x = this.w_h.x - x_y.x;
        if (x_y.y + w_h.y > this.w_h.y)
            w_h.y = this.w_h.y - x_y.y;

        for (int x = x_y.x; x < x_y.x + w_h.x; x++)
        {
            for (int y = x_y.y; y < x_y.y + w_h.y; y++)
            {
                SetTile(
                    new Vector2Int(x, y),
                    TileObj.InstantiateTileObj(tileObjType),
                    roomObj
                );
            }
        }
    }

    public TileObj GetTile(Vector2Int x_y)
    {
        if (!ValidTileSpace(x_y))
            return null;

        return tileObjMatrix[x_y.y, x_y.x];
    }

    public RoomObj GenRoom(RoomObj.RoomObjType roomObjType)
    {
        return RoomObj.InstantiateRoomObj(roomObjType, this);
    }

    public void AddRoom(RoomObj roomObj)
    {
        this.rooms.Add(roomObj);
    }

    /// TODO: this description was from legacy code, change it to fit this (somewhat
    ///       similar) code
    ///  
    /// <summary>
    /// Checks if a coordinate is one that exists within the boundaries of the map/
    /// </summary>
    /// TODO: this belongs in a seperate script as it is not directly tied to the
    ///       concept of GeneratingTiles
    private bool ValidTileSpace(Vector2Int x_y)
    {
        return (x_y.x >= 0 && x_y.y >= 0 && x_y.x < w_h.x && x_y.y < w_h.y);
    }

}
