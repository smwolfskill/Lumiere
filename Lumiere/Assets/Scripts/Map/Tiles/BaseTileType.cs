using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Tile/BaseTile")]
public class BaseTileType : TileType
{
    override public GameObject PopulateGameObject(int x, int y, Map map)
    {
        // Must call parent function first!
        GameObject gameObject = base.PopulateGameObject(x, y, map);
        gameObject.GetComponent<SpriteRenderer>().sprite = sprite;
        gameObject.name = name;

        return gameObject;
    }
}

