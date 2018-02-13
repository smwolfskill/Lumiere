using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class GenerateTiles : MonoBehaviour {

    public int width = 100;
    public int height = 100;
    public double tileOffset = 32.0;

    public GameObject wallTile;

    //TODO: Can a GameObject have a child class? If so, tileMap should
    //      be restricted to holding only Tile subclasses of GameObject.
    //
    // Not only does the Map Prefab hold all tiles as children in the
    // unity heirarchy, but also keeps a reference to them within this
    // 2D array.
    private GameObject[,] tileMap;

	void Start ()
    {
        this.tileMap = new GameObject[height, width];

        GenerateMap();
	}

    private void GenerateMap()
    {
        // Initially cover the map in Wall
        SetTileArea(0, 0, width, height, wallTile);
    }

    /// <summary>
    /// Applies a GameObject to a coordinate in the tileMap as well as adding that tile
    /// to the Unity heirarchy as a child to Map.
    /// </summary>
    /// <returns>
    /// True if the tile was placed correctly, false otherwise (such as when the tile
    /// cannot be placed in the specified coordinate due to the coordinate being invalid)
    /// </returns>
    private bool SetTile(int x, int y, GameObject tile)
    {
        if (!validTileSpace(x, y))
            return false;

        // Instantiate the tile; this takes a Unity prefab (the tile) and generates a 
        // GameObject from the prefab. The generated GameObject is considered to be a
        // clone of the prefab.
        GameObject tileClone = Instantiate(tile, new Vector3(x, y, 0.0f), Quaternion.identity);
        tileClone.transform.parent = gameObject.transform;

        this.tileMap[y, x] = tileClone;

        return true;

    }

    /// <summary>
    /// Sets an area of the map (from left,top to left+width,top+height) all to a 
    /// specified gameTile.
    /// </summary>
    private void SetTileArea(int left, int top, int width, int height, GameObject gameObject)
    {
        for (int x = left; x < left + width; x++)
        {
            for (int y = top; y < top + height; y++)
            {
                SetTile(x, y, gameObject);
            }
        }
    }

    /// <summary>
    /// A function designed to return the tiledata associated with real coordinates of objects.
    /// </summary>
    /// <param name="x">X coordinate in world.</param>
    /// <param name="y">Y coordinate in world.</param>
    /// <returns>The tile at that location. If there is no such tile, returns null.</returns>
    public GameObject getTile(double x, double y)
    {
        // Dummy check.
        if (x < 0 || y < 0)
            return null;

        // Calculate tile based on offset.
        int realX = (int)Math.Floor(x / tileOffset);
        int realY = (int)Math.Floor(y / tileOffset);

        // Dummy check 2.
        if (!validTileSpace(realX, realY))
            return null;

        // Return tile data.
        return this.tileMap[realY, realX];
    }

    /// <summary>
    /// Checks if a coordinate is one that exists within the boundaries of the map/
    /// </summary>
    private bool validTileSpace(int x, int y)
    {
        return (x >= 0 && y >= 0 && x < width && y < height);
    }

}
