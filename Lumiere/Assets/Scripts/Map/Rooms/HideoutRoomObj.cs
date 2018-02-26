using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class HideoutRoomObj : RoomObj
{
    public HideoutRoomObj(Map map) : base(map)
    {
        HideoutRoomType hrt = (HideoutRoomType)gameObject.GetComponent<BaseObjectManager>().baseObject;
        this.x_y = new Vector2Int(
            Utilities.RandomIntInRange(0, map.w_h.x),
            Utilities.RandomIntInRange(0, map.w_h.y)
        );
        this.w_h = new Vector2Int(
            Utilities.RandomIntInRange(hrt.minWidth, hrt.maxWidth),
            Utilities.RandomIntInRange(hrt.minHeight, hrt.maxHeight)
        );
    }

    override protected GameObject PopulateGameObject()
    {
        GameObject gameObject = base.PopulateGameObject();

        gameObject.GetComponent<BaseObjectManager>().baseObject = Object.FindObjectOfType<HideoutRoomType>();

        return gameObject;
    }
}
