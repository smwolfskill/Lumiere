using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class WallTileObj : TileObj
{ 

    public WallTileObj(int x, int y, Map map) : base(x, y, map)
    {
        this.tileObjType = TileObj.TileObjType.WallTileObj;
    }

    override protected GameObject PopulateGameObject()
    {
        // Must call parent function first!
        GameObject gameObject = base.PopulateGameObject();

        BaseObjectManager baseObjectManager = gameObject.GetComponent<BaseObjectManager>();
        WallTileType wallTileType = Resources.Load<WallTileType>("Tiles/WallTile");
        baseObjectManager.baseObject = wallTileType;
        gameObject.GetComponent<SpriteRenderer>().sprite = baseObjectManager.baseObject.sprite;

        gameObject.AddComponent<BoxCollider2D>();

        gameObject.name = "WallTileObj";

        return gameObject;
    }
}
