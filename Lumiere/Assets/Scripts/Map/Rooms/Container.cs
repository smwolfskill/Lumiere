using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Container
{
    public GameObject gameObject;
    protected Map map;
    public List<Tile> tiles;
    public ContainerType containerType;

    public List<Container> connectedContainers;

    public Container(Map map, ContainerType containerType)
    {
        this.map = map;
        this.containerType = containerType;

        tiles = new List<Tile>();

        gameObject = containerType.PopulateGameObject();
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

    public void Remove()
    {
        map.RemoveContainer(this);
        Object.Destroy(gameObject);
    }
}
