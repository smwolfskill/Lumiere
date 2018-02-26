using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Tile/EarthTile")]
public class EarthTileObj : TileObj
{

    public EarthTileObj(Vector2Int x_y, Map map) : base(x_y, map)
    {

    }

    override public GameObject PopulateGameObject()
    {
        GameObject gameObject = base.PopulateGameObject();

        gameObject.AddComponent<BoxCollider2D>();

        return gameObject;
    }
}
