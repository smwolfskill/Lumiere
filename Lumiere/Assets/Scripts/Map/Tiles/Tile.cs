using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Tile
{

    public GameObject gameObject;
    private Map map;
    private Room room;
    public int x, y;
    public TileType tileType;

    public Tile(int x, int y, Map map, TileType tileType)
    {
        this.x = x; this.y = y;
        this.map = map;
        this.tileType = tileType;
        this.gameObject = tileType.PopulateGameObject (x, y, map);
    }

    public void SetX_Y(int x, int y)
    {
        this.x = x; this.y = y;
        gameObject.transform.position = new Vector2 (x, y);
    }

    public void SetRoom(Room room)
    {
        this.room = room;
    }

    public void SetMap(Map map)
    {
        this.map = map;
    }

    public void Remove()
    {
        if (gameObject != null) 
        {
            GameObject.Destroy(gameObject);
        }

        this.room.RemoveTile(this);
    }

    public bool IsWalkable()
    {
        return tileType.walkable;
    }
        
    public Tile GetNeighbor(Utilities.Direction direction)
    {
        switch (direction)
        {
            case Utilities.Direction.NORTH:
                return map.GetTile(x, y - 1);

            case Utilities.Direction.SOUTH:
                return map.GetTile(x, y + 1);

            case Utilities.Direction.WEST:
                return map.GetTile(x - 1, y);

            case Utilities.Direction.EAST:
                return map.GetTile(x + 1, y);
        }
        return null;
    }

    /// <summary>
    /// Gets all the valid neighbors of the tile specified and enumerates them in a list.
    /// </summary>
    /// <returns>The list of neighbors of the tile specified.</returns>
    /// <param name="current">The specified tile.</param>
    public List<Tile> GetNeighbors()
    {
        List<Tile> neighbors = new List<Tile>();
        foreach(Utilities.Direction direction in Enum.GetValues(typeof(Utilities.Direction)))
        {
            Tile neighbor = this.GetNeighbor (direction);
            if (neighbor != null) 
            {
                neighbors.Add(neighbor);
            }

        }

        return neighbors;
    }
}
