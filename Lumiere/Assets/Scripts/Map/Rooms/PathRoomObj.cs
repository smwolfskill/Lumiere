using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PathRoomObj : RoomObj
{

    public PathRoomObj(Map map) : base(map)
    {
        roomObjType = RoomObjType.Path;
    }

    override public GameObject PopulateGameObject()
    {
        // Must call parent function first!
        GameObject gameObject = base.PopulateGameObject();

        gameObject.name = "PathRoomObj";

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
