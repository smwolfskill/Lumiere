using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class RoomObj
{
    public enum RoomObjType
    {
        Hideout,
        Path,
        Blank,
        Secret
    }

    public int x, y;
    public int w, h;

    public GameObject gameObject;
    protected Map map;
    public RoomObjType roomObjType;
    public List<TileObj> tileObjs;

    public RoomObj(Map map)
    {
        this.map = map;

        tileObjs = new List<TileObj>();

        PopulateGameObject();
    }

    public abstract void GenRoom();

    virtual public GameObject PopulateGameObject()
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

    public static RoomObj InstantiateRoomObj(
        RoomObjType roomObjType,
        Map map
    )
    {
        switch (roomObjType)
        {
            case RoomObjType.Hideout:
                return new HideoutRoomObj(map);
            case RoomObjType.Secret:
                return new SecretRoomObj(map);
        }

        return null;
    }

    public void Remove()
    {
        map.RemoveRoom(this);
        Object.Destroy(gameObject);
    }

    public void RefineSize()
    {
        if (w + x > map.w) w = map.w - x;
        if (h + y > map.h) h = map.h - y;
    }
}
