using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class TileObj : BaseObject
{
    private GameObject gameObject;
    private Map map;
    private RoomObj roomObj;
    private TileType tileType;
    public Vector2Int x_y;


    public TileObj(Vector2Int x_y, Map map)
    {
        this.x_y = x_y;
        this.map = map;
    }

    virtual public GameObject PopulateGameObject()
    {
        GameObject gameObject = new GameObject();
        SpriteRenderer sr = gameObject.AddComponent<SpriteRenderer>();
        sr.sprite = sprite;
        gameObject.transform.SetPositionAndRotation(
            new Vector3(x_y.x * map.tileOffset, x_y.y * map.tileOffset),
            Quaternion.identity
        );

        this.gameObject = gameObject;

        return gameObject;
    }

    public void Destory()
    {
        Object.Destroy(gameObject);
    }

    public void SetRoom(RoomObj roomObj)
    {
        this.roomObj = roomObj;
    }
  
}
