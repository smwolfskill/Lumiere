using System;
using UnityEngine;

/// <summary>
/// A class designed to store and maintain data related to the map of tiles and its required components.
/// It is represented as a grid of tiles the size of the world.
/// </summary>
public class GameMap
{
    public const double tileOffset = 32.0;    // Tile size offset in the game's units. A unit is defined by the position logic for objects not locked on the grid.
    public int width;
    public int height;

    private GameTile[,] tileMap;

    /// <summary>
    /// Constructor for the gamemap that includes width and height.
    /// </summary>
    /// <param name="width">Width of the map in tiles.</param>
    /// <param name="height">Height of the map in tiles.</param>
    public GameMap(int width, int height)
    {
        this.width = width;
        this.height = height;

        buildMap();
    }

    /// <summary>
    /// Creates the actual map data, filling tile slots with required information.
    /// </summary>
    private void buildMap()
    {
        this.tileMap = new GameTile[height, width];

        // Call one of many different map creation algorithms
        buildMapSimpleMap();
    }

    /// <summary>
    /// A map creation algorithm.
    /// </summary>
    private void buildMapSimpleMap()
    {
        
    }

    /// <summary>
    /// Applies a GameTile to a coordinate in the tileMap.
    /// </summary>
    /// <returns>
    /// True if the tile was placed correctly, false otherwise (such as when the tile
    /// cannot be placed in the specified coordinate due to the coordinate being invalid)
    /// </returns>
    private bool setTile(int x, int y, GameTile gameTile)
    {
        if (!validTileSpace(x, y))
            return false;

        this.tileMap[y, x] = gameTile;

        return true;
    }

    /// <summary>
    /// Sets an area of the map (from left,top to left+width,top+height) all to a 
    /// specified gameTile.
    /// </summary>
    private void setTileArea(int left, int top, int width, int height, GameTile gameTile)
    {
        for (int x = left; x < left + width; x++)
        {
            for (int y = top; y < top + height; y++)
            {
                setTile(x, y, gameTile);
            }
        }
    }

    /// <summary>
    /// Basic getter function for the map's height.
    /// </summary>
    /// <returns>The map's height in tiles.</returns>
    public int getHeight()
    {
        return this.height;
    }

    /// <summary>
    /// Basic getter function for the map's width.
    /// </summary>
    /// <returns>The map's width in tiles.</returns>
    public int getWidth()
    {
        return this.width;
    }

    /// <summary>
    /// A function designed to return the tiledata associated with real coordinates of objects.
    /// </summary>
    /// <param name="x">X coordinate in world.</param>
    /// <param name="y">Y coordinate in world.</param>
    /// <returns>The tile at that location. If there is no such tile, returns null.</returns>
    public GameTile getTile(double x, double y)
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
