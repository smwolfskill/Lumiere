using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SecretRoomObj : RoomObj
{

    private SecretRoomType srt;

    public SecretRoomObj(Map map) : base(map)
    {
        roomObjType = RoomObjType.Secret;

        srt = (SecretRoomType)gameObject.GetComponent<BaseObjectManager>().baseObject;
        this.x = Utilities.RandomIntInRange(0, map.w);
        this.y = Utilities.RandomIntInRange(0, map.h);
        this.w = srt.width_height;
        this.h = srt.width_height;

        RefineSize();
    }

    override public GameObject PopulateGameObject()
    {
        // Must call parent function first!
        GameObject gameObject = base.PopulateGameObject();

        gameObject.GetComponent<BaseObjectManager>().baseObject = Resources.Load<SecretRoomType>("Rooms/Secret");

        gameObject.name = "SecretRoomObj";

        return gameObject;
    }

    public override void GenRoom()
    {
        map.FillArea(x, y, w, h, TileObj.TileObjType.SandTileObj, this);

        map.FillLine(x, y, w, Utilities.Direction.EAST, TileObj.TileObjType.WallTileObj, this);
        map.FillLine(x, y, h, Utilities.Direction.SOUTH, TileObj.TileObjType.WallTileObj, this);
        map.FillLine(x, y + h - 1, w, Utilities.Direction.EAST, TileObj.TileObjType.WallTileObj, this);
        map.FillLine(x + w - 1, y, h, Utilities.Direction.SOUTH, TileObj.TileObjType.WallTileObj, this);
        map.SetTile(x + w - 1, y + h - 1, new WallTileObj(x + w - 1, y + h - 1, this.map), this);
    }

    protected override RoomType GetRoomType()
    {
        return srt;
    }
}
