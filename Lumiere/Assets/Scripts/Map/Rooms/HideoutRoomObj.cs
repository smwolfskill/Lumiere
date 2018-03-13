using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class HideoutRoomObj : RoomObj
{

    private HideoutRoomType hrt;

    public HideoutRoomObj(Map map) : base(map)
    {
        roomObjType = RoomObjType.Hideout;

        hrt = (HideoutRoomType)gameObject.GetComponent<BaseObjectManager>().baseObject;
        this.x = Utilities.RandomIntInRange(0, map.w);
        this.y = Utilities.RandomIntInRange(0, map.h);
        this.w = Utilities.RandomIntInRange(hrt.minWidth, hrt.maxWidth);
        this.h = Utilities.RandomIntInRange(hrt.minHeight, hrt.maxHeight);

        RefineSize();

    }

    override public GameObject PopulateGameObject()
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
        map.FillLine(x, y, w, Utilities.Direction.EAST, TileObj.TileObjType.WallTileObj, this);
        map.FillLine(x, y, h, Utilities.Direction.SOUTH, TileObj.TileObjType.WallTileObj, this);
        map.FillLine(x, y+h-1, w, Utilities.Direction.EAST, TileObj.TileObjType.WallTileObj, this);
        map.FillLine(x+w-1, y, h, Utilities.Direction.SOUTH, TileObj.TileObjType.WallTileObj, this);
        map.SetTile(x + w - 1, y + h - 1, new WallTileObj(x+w - 1, y+h - 1, this.map), this);
    }

    /// <summary>
    /// Spawns the player inside the hideout room.
    /// </summary>
    public override void SpawnPlayer()
    {
        //find location, spawn player
        List<TileObj> walkableTiles = GetWalkableTiles ();
        List<Vector2Int> spawnLocations = new List<Vector2Int> ();
        TileObj walkableTile = walkableTiles[Utilities.RandomIntInRange (0, walkableTiles.Count)];
        Vector2Int tileLocation = new Vector2Int (walkableTile.x, walkableTile.y);
        Player playerToSpawn = hrt.player;

        // Multiply location by tile offset to account for tile spacing or tile sizes
        Vector2 locationToSpawn = new Vector2 (tileLocation.x * map.tileOffset, tileLocation.y * map.tileOffset);
        playerToSpawn.Spawn (map, locationToSpawn);
    }
}
