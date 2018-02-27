using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class FloorTileObj : TileObj
{

    public FloorTileObj(int x, int y, Map map) : base(x, y, map)
    {
        this.tileObjType = TileObj.TileObjType.FloorTileObj;
    }

    override protected GameObject PopulateGameObject()
    {
        // Must call parent function first!
        GameObject gameObject = base.PopulateGameObject();

        BaseObjectManager baseObjectManager = gameObject.GetComponent<BaseObjectManager>();
        FloorTileType floorTileType = Resources.Load<FloorTileType>("Tiles/FloorTile");
        baseObjectManager.baseObject = floorTileType;
        gameObject.GetComponent<SpriteRenderer>().sprite = baseObjectManager.baseObject.sprite;

        gameObject.name = "FloorTileObj";

        return gameObject;
    }
}
