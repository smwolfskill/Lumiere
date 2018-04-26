using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class TileType : BaseObject
{
    public bool walkable;

    virtual public GameObject PopulateGameObject(int x, int y, Map map)
    {
        GameObject gameObject = new GameObject(name);
        SpriteRenderer sr = gameObject.AddComponent<SpriteRenderer>();
        gameObject.transform.position = new Vector2(x * map.tileOffset, y * map.tileOffset);

        return gameObject;
    }

}
