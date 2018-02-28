using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class TileObj
{
    public enum TileObjType
    {
        EarthTileObj,
        WallTileObj,
        FloorTileObj,
        SandTileObj
    }

    public GameObject gameObject;
    private Map map;
    private RoomObj roomObj;
    public int x, y;
    public TileObjType tileObjType;

    public TileObj(int x, int y, Map map)
    {
        this.x = x; this.y = y;
        this.map = map;

        PopulateGameObject();
    }

    public void SetX_Y(int x, int y)
    {
        this.x = x; this.y = y;
    }

    public void SetRoom(RoomObj roomObj)
    {
        this.roomObj = roomObj;
    }

    public void SetMap(Map map)
    {
        this.map = map;
    }

    virtual protected GameObject PopulateGameObject()
    {
        GameObject gameObject = new GameObject("TileObj");
        SpriteRenderer sr = gameObject.AddComponent<SpriteRenderer>();
        BaseObjectManager baseObjectManager = gameObject.AddComponent<BaseObjectManager>();
        gameObject.transform.SetPositionAndRotation(
            new Vector3(x * map.tileOffset, y * map.tileOffset),
            Quaternion.identity
        );

        this.gameObject = gameObject;

        return gameObject;
    }

    public void Remove()
    {
        Object.Destroy(gameObject);
        this.roomObj.RemoveTile(this);
    }

    public static TileObj InstantiateTileObj(
        int x, int y,
        Map map,
        TileObjType tileObjType
    )
    {
        switch(tileObjType)
        {
            case TileObjType.EarthTileObj:
                return new EarthTileObj(x, y, map);
            case TileObjType.WallTileObj:
                return new WallTileObj(x, y, map);
            case TileObjType.FloorTileObj:
                return new FloorTileObj(x, y, map);
            case TileObjType.SandTileObj:
                return new SandTileObj(x, y, map);
        }

        return null;
    }

    public TileObj GetNeighbor(Utilities.Direction direction)
    {
        switch (direction)
        {
            case Utilities.Direction.NORTH:
                return map.GetTile(x, y - 1);

            case Utilities.Direction.SOUTH:
                return map.GetTile(x, y + 1);

            case Utilities.Direction.WEST:
                return map.GetTile(x - 1, y);

            case Utilities.Direction.EAST:
                return map.GetTile(x + 1, y);
        }
        return null;
    }
}
