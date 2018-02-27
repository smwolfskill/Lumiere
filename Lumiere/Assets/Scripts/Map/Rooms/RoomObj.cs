using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class RoomObj
{


    public int x, y;
    public int w, h;

    public GameObject gameObject;
    protected Map map;

    public List<TileObj> tileObjs;

    public RoomObj(Map map)
    {
        this.map = map;

        tileObjs = new List<TileObj>();

        PopulateGameObject();
        gameObject.transform.parent = this.map.gameObject.transform;
    }

    public abstract void GenRoom();

    virtual protected GameObject PopulateGameObject()
    {
        GameObject gameObject = new GameObject("RoomObj");
        this.gameObject = gameObject;
        this.gameObject.AddComponent<BaseObjectManager>();
        return gameObject;
    }

    public void AddTile(TileObj tileObj)
    {
        tileObj.gameObject.transform.parent = gameObject.transform;

        tileObjs.Add(tileObj);
    }

    public void RemoveTile(TileObj tileObj)
    {
        tileObjs.Remove(tileObj);
    }

    protected List<TileObj> GetWalkableTiles()
    {
        List<TileObj> walkableTiles = new List<TileObj> ();
        foreach (TileObj tileObj in tileObjs) 
        {
            if (tileObj.IsWalkable ()) 
            {
                walkableTiles.Add (tileObj);
            }
        }

        return walkableTiles;
    }

    public enum RoomObjType
    {
        Hideout,
        Blank,
        Entity
    }

    public static RoomObj InstantiateRoomObj(
        RoomObjType roomObjType,
        Map map
    )
    {
        switch (roomObjType)
        {
            case RoomObjType.Hideout:
                return new HideoutRoomObj(map);
            case RoomObjType.Entity:
                return new EntityRoomObj (map);
        }

        return null;
    }

    public void RefineSize()
    {
        if (w + x > map.w) w = map.w - x;
        if (h + y > map.h) h = map.h - y;
    }
}
