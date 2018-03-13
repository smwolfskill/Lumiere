using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BlankRoomObj : RoomObj
{
    public BlankRoomObj(int x, int y, int w, int h, Map map) : base(map)
    {
        this.x = x;
        this.y = y;
        this.w = w;
        this.h = h;

        roomObjType = RoomObjType.Blank;
    }

    override public GameObject PopulateGameObject()
    {
        GameObject gameObject = base.PopulateGameObject();

        gameObject.name = "BlankRoomObj";

        return gameObject;
    }

    public override void GenRoom()
    {
        
    }

    protected override RoomType GetRoomType()
    {
        throw new System.NotImplementedException();
    }
}
