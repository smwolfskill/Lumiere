﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class RoomObj
{
    public enum RoomObjType
    {
        Hideout,
        Path,
        Blank,
        Secret,
        Entity
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

    public virtual PlayerObject SpawnPlayer ()
    {
        //find location, spawn player
        List<TileObj> walkableTiles = GetWalkableTiles();
        TileObj walkableTile = walkableTiles[Utilities.RandomIntInRange(0, walkableTiles.Count)];
        Vector2Int tileLocation = new Vector2Int(walkableTile.x, walkableTile.y);
        Player playerToSpawn = GetRoomType().player;

        // Multiply location by tile offset to account for tile spacing or tile sizes
        Vector2 locationToSpawn = new Vector2(tileLocation.x * map.tileOffset, tileLocation.y * map.tileOffset);
        GameObject playerGameObject = playerToSpawn.Spawn(map, locationToSpawn);

        return (PlayerObject)playerGameObject.GetComponent<EntityObjectManager>().entityObject;
    }

    abstract protected RoomType GetRoomType();

    virtual public GameObject PopulateGameObject()
    {
        this.gameObject = new GameObject("RoomObj");
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
            case RoomObjType.Entity:
                return new EntityRoomObj (map);
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
