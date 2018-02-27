using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EntityRoomObj : RoomObj 
{
    EntityRoomType entityRoomType;
    int numEntities;

    public EntityRoomObj(Map map) : base(map)
    {
        entityRoomType = (EntityRoomType) gameObject.GetComponent<BaseObjectManager>().baseObject;
        numEntities = Utilities.RandomIntInRange (entityRoomType.minimumEntities, entityRoomType.maximumEntities);
        this.x = Utilities.RandomIntInRange(0, map.w);
        this.y = Utilities.RandomIntInRange(0, map.h);
        this.w = Utilities.RandomIntInRange(entityRoomType.minWidth, entityRoomType.maxWidth);
        this.h = Utilities.RandomIntInRange(entityRoomType.minHeight, entityRoomType.maxHeight);

        RefineSize();
    }

    override protected GameObject PopulateGameObject()
    {
        // Must call parent function first!
        GameObject gameObject = base.PopulateGameObject();

        gameObject.GetComponent<BaseObjectManager>().baseObject = Resources.Load<EntityRoomType>("Rooms/Entity");

        gameObject.name = "EntityRoomObj";

        return gameObject;
    }


    protected void SpawnEntities()
    {
        List<TileObj> walkableTiles = GetWalkableTiles ();
        List<Vector2> spawnLocations = new List<Vector2> ();
        Entity[] entities = entityRoomType.entities;
        int entitiesSpawned = 0;

        while (entitiesSpawned < numEntities) 
        {
            Debug.Log ("Number of tiles: " + tileObjs.Count);
            Debug.Log ("Walkable Tiles: " + walkableTiles.Count);
            TileObj walkableTile = walkableTiles[Utilities.RandomIntInRange (0, walkableTiles.Count)];
            Entity entityToSpawn = entities[Utilities.RandomIntInRange (0, entities.Length)];
            Vector2 tileLocation = new Vector2 (walkableTile.x, walkableTile.y);

            if (!spawnLocations.Contains (tileLocation)) 
            {
                Vector2 locationToSpawn = new Vector2 (tileLocation.x * map.tileOffset, tileLocation.y * map.tileOffset);
                entityToSpawn.Spawn (locationToSpawn);
                spawnLocations.Add (tileLocation);
                entitiesSpawned++;
            }
        }

    }

    public override void GenRoom()
    {
        map.FillArea(x, y, w, h, TileObj.TileObjType.FloorTileObj, this);
        //tileObjs.AddRange (addedTiles);
        SpawnEntities ();
    }

}
