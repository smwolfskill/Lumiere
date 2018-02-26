using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EarthTileObj : TileObj
{

    public EarthTileObj() : base()
    {

    }

    override protected GameObject PopulateGameObject()
    {
        GameObject gameObject = base.PopulateGameObject();

        gameObject.GetComponent<EntitySpriteManager>().entity = Object.FindObjectOfType<EarthTileType>();

        gameObject.AddComponent<BoxCollider2D>();

        return gameObject;
    }
}
