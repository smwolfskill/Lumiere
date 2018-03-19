using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Tile/ColliderTile")]
public class ColliderTileType : TileType
{
    override public GameObject PopulateGameObject(int x, int y, Map map)
    {
        GameObject gameObject = base.PopulateGameObject(x, y, map);
        gameObject.GetComponent<SpriteRenderer> ().sprite = sprite;
        gameObject.AddComponent<BoxCollider2D>();
        gameObject.name = name;

        return gameObject;
    }
}
