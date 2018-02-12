using System;
using UnityEngine;

/// <summary>
/// A class designed to store and maintain data related to the map of tiles and its required components.
/// It is represented as a grid of tiles the size of the world.
/// </summary>
public class GameMap
{
    private const double tileOffset = 32.0;    // Tile size offset in the game's units. A unit is defined by the position logic for objects not locked on the grid.
    private GameTile[,] tileMap;
    private int width;
    private int height;
    
    /// <summary>
    /// Constructor for the gamemap that includes width and height.
    /// </summary>
    /// <param name="width">Width of the map in tiles.</param>
    /// <param name="height">Height of the map in tiles.</param>
    public GameMap(int width, int height)
    {
        buildMap(width, height);
    }

    /// <summary>
    /// Creates the actual map data, filling tile slots with required information.
    /// </summary>
    /// <param name="width">Width of the map in tiles.</param>
    /// <param name="height">Height of the map in tiles.</param>
    private void buildMap(int width, int height)
    {
        this.width = width;
        this.height = height;
        this.tileMap = new GameTile[height, width];
        // TODO: Call map generator function to establish array.
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
        if (realX > this.width || realY > this.height)
            return null;
        
        // Return tile data.
        return this.tileMap[realY, realX];
    }
}
