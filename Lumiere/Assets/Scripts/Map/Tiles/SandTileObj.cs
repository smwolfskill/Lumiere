using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SandTileObj : TileObj
{

    public SandTileObj(int x, int y, Map map) : base(x, y, map)
    {
        this.tileObjType = TileObj.TileObjType.SandTileObj;
    }

    override protected GameObject PopulateGameObject()
    {
        // Must call parent function first!
        GameObject gameObject = base.PopulateGameObject();

        BaseObjectManager baseObjectManager = gameObject.GetComponent<BaseObjectManager>();
        SandTileType sandTileType = Resources.Load<SandTileType>("Tiles/SandTile");
        baseObjectManager.baseObject = sandTileType;
        gameObject.GetComponent<SpriteRenderer>().sprite = baseObjectManager.baseObject.sprite;

        gameObject.name = "SandTileObj";

        return gameObject;
    }

    public override bool IsWalkable ()
    {
        return true;
    }

}
