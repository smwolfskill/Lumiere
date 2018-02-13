using System;
using UnityEngine;

public class GenerateTiles : MonoBehaviour {

    public enum GenerationAlgorithm
    {
        SIMPLE
    };

    public GenerationAlgorithm generationAlgorithm;

    [Header("GenAlgo Simple")]
    public int roomAttempts = 30;

    [Header("GenAlgo Constraints")]
    public int smallestRoomDim = 5; //inclusive
    public int largestRoomDim = 15; //inclusive

    [Header("Sizing")]
    public int width = 100;
    public int height = 100;
    public float tileOffset = 1.0f;

    [Header("Tiles")]
    public GameObject wallTile;
    public GameObject floorTile;
    public GameObject earthTile;

    // Random number generator
    private System.Random random;

    //TODO: Can a GameObject have a child class? If so, tileMap should
    //      be restricted to holding only Tile subclasses of GameObject.
    //
    // Not only does the Map Prefab hold all tiles as children in the
    // unity heirarchy, but also keeps a reference to them within this
    // 2D array.
    private GameObject[,] tileMap;

    void Start()
    {
        random = new System.Random();

        this.tileMap = new GameObject[height, width];

        switch (generationAlgorithm)
        {
            case GenerationAlgorithm.SIMPLE:
                GenerateMapSimple();
                break;
        }

    }

    private void GenerateMapSimple()
    {
        // Initially cover the map in Earth
        SetTileArea(0, 0, width, height, earthTile);

        for (int roomAttempt = 0; roomAttempt < roomAttempts; roomAttempt++)
        {
            AttemptGenRandomRoom();
        }


    }

    /// <summary>
    /// Attempting to generate a room that is randomly placed in the map.
    /// </summary>
    /// <returns>
    /// False when a room is not created, True otherwise
    /// </returns>
    private bool AttemptGenRandomRoom()
    {
        int roomWidth = random.Next(smallestRoomDim, largestRoomDim + 1);
        int roomHeight = random.Next(smallestRoomDim, largestRoomDim + 1);

        int left = random.Next(0, this.width - roomWidth);
        int top = random.Next(0, this.height - roomHeight);

        print(""+left+", "+top+", "+roomWidth+", "+roomHeight+"");

        TileAttributes.TileType[] nonGroundTiles = { TileAttributes.TileType.FLOOR, TileAttributes.TileType.WALL };
        // Verify this area is not overlapping non ground tiles.
        bool isRoomPossible = !IsTileTypeInRect(left, top, roomWidth, roomHeight, nonGroundTiles);
        if(isRoomPossible)
        {
            SetTileArea(left, top, roomWidth, roomHeight, floorTile, wallTile);
        }

        return isRoomPossible;
    }


    /// <summary>
    /// Applies a GameObject to a coordinate in the tileMap as well as adding that tile
    /// to the Unity heirarchy as a child to Map. Removes the tile that existed in the
    /// specified location if a tile exists.
    /// </summary>
    /// <returns>
    /// True if the tile was placed correctly, false otherwise (such as when the tile
    /// cannot be placed in the specified coordinate due to the coordinate being invalid)
    /// </returns>
    private bool SetTile(int x, int y, GameObject tile)
    {
        if (!validTileSpace(x, y))
            return false;

        // Remove a tile from the Unity hierarchy if one is existing in this tile location.
        GameObject preExistingTile = GetTile(x, y);
        if (preExistingTile)
            Destroy(preExistingTile);

        // Instantiate the tile; this takes a Unity prefab (the tile) and generates a 
        // GameObject from the prefab. The generated GameObject is considered to be a
        // clone of the prefab.
        GameObject tileClone = Instantiate(tile, new Vector3(x*tileOffset, y*tileOffset, 0.0f), Quaternion.identity);
        tileClone.transform.parent = gameObject.transform;

        this.tileMap[y, x] = tileClone;

        return true;

    }

    /// <summary>
    /// Returns the TileType of a Tile. A TileType is a value in the enum TileAttributes.TileType.
    /// </summary>
    private TileAttributes.TileType GetTileType(GameObject tile)
    {
        return tile.GetComponent<TileAttributes>().tileType;
    }

    /// <summary>
    /// Checks if the given tile's TileType matches the given tileType
    /// </summary>
    /// <param name="tile">A GameObject holding the TileAttributes script</param>
    /// <param name="tileType">A value from the enum TileAttributes.TileType</param>
    /// <returns>True when tile's TileType matches tileType, False otherwise</returns>
    private bool IsTileTypeOf(GameObject tile, TileAttributes.TileType tileType)
    {
        return GetTileType(tile) == tileType;
    }

    /// <summary>
    /// An extension of bool IsTileTypeOf(GameObject tile, TileAttributes.TileType tileType)
    /// that allows for checking if the given tile is one of many different tileTypes.
    /// </summary>
    /// <param name="tile">A GameObject holding the TileAttributes script</param>
    /// <param name="tileTypes">An array of TileAttribute.TileTypes</param>
    /// <returns>True when tile has any of the passed in tileTypes, False otherwise</returns>
    private bool IsTileTypeOf(GameObject tile, TileAttributes.TileType[] tileTypes)
    {
        foreach(TileAttributes.TileType tileType in tileTypes)
        {
            bool ret = IsTileTypeOf(tile, tileType);
            if (ret)
                return true;
        }
        return false;
    }

    private bool IsTileTypeInRect(int left, int top, int width, int height, TileAttributes.TileType tileType)
    {
        TileAttributes.TileType[] tileTypeArr = { tileType };
        return IsTileTypeInRect(left, top, width, height, tileTypeArr);
    }

    private bool IsTileTypeInRect(int left, int top, int width, int height, TileAttributes.TileType[] tileTypes)
    {
        for (int x = left; x < left + width; x++)
        {
            for (int y = top; y < top + height; y++)
            {
                bool ret = IsTileTypeOf(GetTile(x, y), tileTypes);
                if (ret)
                    return true;
            }
        }

        return false;
    }

    private GameObject GetTile(int x, int y)
    {
        if (!validTileSpace(x, y))
            return null;

        return this.tileMap[y, x];
    }

    /// <summary>
    /// Sets an area of the map (from left,top to left+width,top+height) all to a 
    /// specified gameTile.
    /// </summary>
    private void SetTileArea(int left, int top, int width, int height, GameObject fillGameObject, GameObject borderGameObject = null)
    {
        // Force top and left of rectangle to be inside the map.
        if (left < 0)
            left = 0;
        if (top < 0)
            top = 0;

        // Force width and height of rectangle to not be outside the map.
        if (left + width > this.width)
            width = this.width - left;
        if (top + height > this.height)
            height = this.height - top;

        for (int x = left; x < left + width; x++)
        {
            for (int y = top; y < top + height; y++)
            {
                // Check to see if a border was provided; if so, then check to see if we
                // are currently placing a border obj; if so, then place the border.
                if (borderGameObject &&
                    (x == left || x == left + width - 1 || y == top || y == top + height - 1)
                )
                {
                    SetTile(x, y, borderGameObject);
                }
                // Otherwise, place the default fill GameObject
                else
                {
                    SetTile(x, y, fillGameObject);
                }
            }
        }
    }

    /// <summary>
    /// A function designed to return the tiledata associated with real coordinates of objects.
    /// </summary>
    /// <param name="x">X coordinate in world.</param>
    /// <param name="y">Y coordinate in world.</param>
    /// <returns>The tile at that location. If there is no such tile, returns null.</returns>
    public GameObject getTile(float x, float y)
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
