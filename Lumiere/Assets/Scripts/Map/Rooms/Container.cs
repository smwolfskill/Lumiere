﻿using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Container
{
    public GameObject gameObject;
    protected Map map;
    public List<Tile> tiles;
    public ContainerType containerType;
    public List<Door> doors;


    public List<Container> connectedContainers;

    public Container(Map map, ContainerType containerType)
    {
        this.map = map;
        this.containerType = containerType;

        connectedContainers = new List<Container>();
        connectedContainers.Add(this);

        tiles = new List<Tile>();

        gameObject = containerType.PopulateGameObject();

        this.doors = new List<Door>();

    }

    public void AddTile(Tile tile)
    {
        tile.gameObject.transform.parent = gameObject.transform;

        tiles.Add(tile);
    }

    public void RemoveTile(Tile tile)
    {
        tiles.Remove(tile);
    }

    public List<Tile> GetWalkableTiles()
    {
        List<Tile> walkableTiles = new List<Tile>();
        foreach (Tile tile in tiles)
        {
            if (tile.IsWalkable())
            {
                walkableTiles.Add(tile);
            }
        }

        return walkableTiles;
    }

    public List<Tile> GetTilesOfType(TileType tileType)
    {
        List<Tile> tilesOfType = new List<Tile>();
        foreach (Tile tile in tiles)
        {
            if (tile.tileType == tileType)
                tilesOfType.Add(tile);
        }

        return tilesOfType;
    }


    public void Remove()
    {
        map.RemoveContainer(this);
        Object.Destroy(gameObject);
    }

    public bool HasTile(Tile tile)
    {
        return tiles.Contains(tile);
    }

    public bool HasTileType(TileType tileType)
    {
        foreach (Tile tile in tiles)
        {
            if (tile.tileType == tileType)
                return true;
        }

        return false;
    }
}
