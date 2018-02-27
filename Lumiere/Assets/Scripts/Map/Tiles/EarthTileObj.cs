using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EarthTileObj : TileObj
{

    public EarthTileObj(int x, int y, Map map) : base(x, y, map)
    {
        this.tileObjType = TileObj.TileObjType.EarthTileObj;
    }

    override protected GameObject PopulateGameObject()
    {
        // Must call parent function first!
        GameObject gameObject = base.PopulateGameObject();

        // Pretty confusing, so here we go. By Unity's Entity Programming Paradigm, instantiated
        // objects should have a ScirptableObject that controls that object's properties. In this
        // case, the EarthTileObj's one property is its sprite.
        // We first obtain the baseObjectManager that was applied to the gameObject in the parent
        // of this function. Once we have the baseObjectManager, it is used to holder the baseObject.
        // The baseObject in our case here is the earthTileType. There exists only one instance of
        // earthTileType while there exist many instances of earthTileObj. All earthTileObjs inherit
        // their properties (in this case 1, the sprite) from earthTileType. To obtain the properties
        // from earthTileType, we need to place the earthTileType into the earthTileObj's gameObject's
        // baseObjectManager's baseObject property. Once done, we can set the earthTileType's sprite
        // as the earthTileObj's SpriteRenderer's sprite.
        //
        // Ask Wyatt for more explanation if needed.
        BaseObjectManager baseObjectManager = gameObject.GetComponent<BaseObjectManager>();
        EarthTileType earthTileType = Resources.Load<EarthTileType>("Tiles/EarthTile");
        baseObjectManager.baseObject = earthTileType;
        gameObject.GetComponent<SpriteRenderer>().sprite = baseObjectManager.baseObject.sprite;

        gameObject.AddComponent<BoxCollider2D>();

        gameObject.name = "EarthTileObj";

        return gameObject;
    }

    public override bool IsWalkable ()
    {
        return false;
    }
}
