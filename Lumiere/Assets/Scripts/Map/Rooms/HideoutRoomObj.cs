using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class HideoutRoomObj : RoomObj
{
    public HideoutRoomObj(Map map) : base(map)
    {
        HideoutRoomType hrt = (HideoutRoomType)gameObject.GetComponent<BaseObjectManager>().baseObject;
        this.x = Utilities.RandomIntInRange(0, map.w);
        this.y = Utilities.RandomIntInRange(0, map.h);
        this.w = Utilities.RandomIntInRange(hrt.minWidth, hrt.maxWidth);
        this.h = Utilities.RandomIntInRange(hrt.minHeight, hrt.maxHeight);

        RefineSize();
    }

    override protected GameObject PopulateGameObject()
    {
        // Must call parent function first!
        GameObject gameObject = base.PopulateGameObject();

        gameObject.GetComponent<BaseObjectManager>().baseObject = Resources.Load<HideoutRoomType>("Rooms/Hideout");

        gameObject.name = "HideoutRoomObj";

        return gameObject;
    }

    public override void GenRoom()
    {
        map.FillArea(x, y, w, h, TileObj.TileObjType.FloorTileObj, this);
    }
}
