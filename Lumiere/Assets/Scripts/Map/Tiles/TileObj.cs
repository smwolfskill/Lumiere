using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class TileObj
{

    public GameObject gameObject;
    private Map map;
    private RoomObj roomObj;
    public Vector2Int x_y;

    public TileObj()
    {
        PopulateGameObject();
    }

    public void SetX_Y(Vector2Int x_y)
    {
        this.x_y = x_y;
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
        GameObject gameObject = new GameObject();
        SpriteRenderer sr = gameObject.AddComponent<SpriteRenderer>();
        EntitySpriteManager esm = gameObject.AddComponent<EntitySpriteManager>();
        gameObject.transform.SetPositionAndRotation(
            new Vector3(x_y.x * map.tileOffset, x_y.y * map.tileOffset),
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

    public enum TileObjType
    {
        EarthTileObj,
    }

    public static TileObj InstantiateTileObj(
        TileObjType tileObjType
    )
    {
        switch(tileObjType)
        {
            case TileObjType.EarthTileObj:
                return new EarthTileObj();
        }

        return null;
    }
}
