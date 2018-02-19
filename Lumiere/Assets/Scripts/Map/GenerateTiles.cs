using System;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// Generator class for handling tile generation.
/// </summary>
public class GenerateTiles : MonoBehaviour
{

    public enum GenerationAlgorithm
    {
        SIMPLE
    };


    public GenerationAlgorithm generationAlgorithm;

    [Header("GenAlgo Simple")]
    public int roomAttempts = 30;
    public int pathAttempts = 30;
    public int pathDirectionChangeLikelihood = 10; // Larger means less likely to change directions.

    [Header("GenAlgo Constraints")]
    public int smallestRoomDim = 5; // inclusive
    public int largestRoomDim = 15; // inclusive

    [Header("Sizing")]
    public int width = 100;
    public int height = 100;
    public float tileOffset = 1.0f;

    [Header("Tiles")]
    public GameObject wallTile;
    public GameObject floorTile;
    public GameObject earthTile;

    [Header("Containers")]
    public GameObject roomContainer;
    public GameObject earthContainer;
    public GameObject pathContainer;

    // Random number generator
    private System.Random random;

    //TODO: Can a GameObject have a child class? If so, tileMap should
    //      be restricted to holding only Tile subclasses of GameObject.
    //
    // Not only does the Map Prefab hold all tiles as children in the
    // unity heirarchy, but also keeps a reference to them within this
    // 2D array.
    private GameObject[,] tileMap;

    private List<GameObject> containers;

    void Start()
    {
        random = new System.Random();

        this.tileMap = new GameObject[height, width];
        this.containers = new List<GameObject>();

        switch (generationAlgorithm)
        {
            case GenerationAlgorithm.SIMPLE:
                GenerateMapSimple();
                break;
        }

    }

    /// <summary>
    /// The GenerateMapSimple algorithm first places at most roomAttempts rooms in the map, then
    /// places at most pathAttempts paths connecting the rooms. Paths are started from a side of
    /// a room and continue until the path hits a room. 
    /// </summary>
    private void GenerateMapSimple()
    {
        // Initially cover the map in Earth
        SetTileArea(0, 0, width, height, earthTile, null, InstantiateContainer(earthContainer));

        for (int roomAttempt = 0; roomAttempt < roomAttempts; roomAttempt++)
        {
            AttemptGenRandomRoom();
        }

        for(int pathAttempt = 0; pathAttempt < pathAttempts; pathAttempt++)
        {
            AttemptGenRandomPath();
        }
    }

    private void AttemptGenRandomPath()
    {
        // The container that will hold all tiles related to this path.
        GameObject currPathContainer = InstantiateContainer(pathContainer);

        GameObject roomContainer = GetRandomContainer(ContainerAttributes.ContainerType.ROOM);
        ContainerAttributes roomContainerAttributes = roomContainer.GetComponent<ContainerAttributes>();

        // These values should be refactored into a struct that exists in an abstract utilities class.
        int left = roomContainerAttributes.GetLeft();
        int top = roomContainerAttributes.GetTop();
        int width = roomContainerAttributes.GetWidth();
        int height = roomContainerAttributes.GetHeight();

        GameObject startingTile = null;

        // Obtain a random direction to start creating a path in. This direction will also be the
        // side of the room that the path starts generating from (ex: North = start making a path 
        // from the North side of a room)
        Utilities.Direction startingDirection = GetRandomEnumValue<Utilities.Direction>();

        // Based on the startingDirection choose a startingTile.
        switch(startingDirection)
        {
            case Utilities.Direction.NORTH:

                // We want to choose a tile that is not in a corner and that is on the
                // north wall of the room.
                startingTile = GetTile(random.Next(left + 1, left + width - 1), top);
                break;

            case Utilities.Direction.SOUTH:
                startingTile = GetTile(random.Next(left + 1, left + width - 1), top + height - 1);
                break;

            case Utilities.Direction.WEST:
                startingTile = GetTile(left, random.Next(top + 1, top + height - 1));
                break;

            case Utilities.Direction.EAST:
                startingTile = GetTile(left + width - 1, random.Next(top + 1, top + height - 1));
                break;
        }


        AttemptGenRandomPathStep(startingTile, startingDirection, currPathContainer);
    }

    private void AttemptGenRandomPathStep(GameObject tile, Utilities.Direction direction, GameObject currPathContainer)
    {
        // If the tile is null (this is possible when a path is being generated and 
        // ends up past an edge of the map), stop creating the path.
        if (tile == null)
            return;

        TileAttributes tileAttributes = tile.GetComponent<TileAttributes>();

        // Once the path has found a floor, stop creating the path.
        if (tileAttributes.tileType == TileAttributes.TileType.FLOOR)
            return;

        // Overwrite passed in tile with floor
        SetTile(tileAttributes.GetX(), tileAttributes.GetY(), floorTile, currPathContainer);

        // Test if a random direction change should occur. A 0 will lead to a left
        // left direction change; 1 will lead to a right direction change, all else
        // will lead to going the same direction. This can be refactored into assigning
        // 0 and 1 to an enum or constants.
        int directionChangeVal = random.Next(0, pathDirectionChangeLikelihood);
        if(directionChangeVal == 0)
        {
            direction = Utilities.LeftOf(direction);
        }
        else if(directionChangeVal == 1)
        {
            direction = Utilities.RightOf(direction);
        }

        // Take another step in creating the path.
        AttemptGenRandomPathStep(tileAttributes.GetNeighbor(direction), direction, currPathContainer);
        
    }

    // refactor: Should be made public static in a public utility class.
    private T GetRandomEnumValue<T>()
    {
        Type type = typeof(T);
        Array values = Enum.GetValues(type);
        return (T)values.GetValue(random.Next(0, values.Length));
    }

    private GameObject GetRandomContainer(ContainerAttributes.ContainerType containerType)
    {
        GameObject container;

        // Continually search for a container of a containerType until one is found, then
        // return that container.
        //
        // WARNING: currently this function will not halt unless a container of containerType
        //          exists in containers.
        do
        {
            container = containers[random.Next(0, containers.Count)];
        }
        while (!IsContainerTypeOf(container, containerType));

        return container;
    }

    private bool IsContainerTypeOf(GameObject container, ContainerAttributes.ContainerType containerType)
    {
        return container.GetComponent<ContainerAttributes>().containerType == containerType;
    }

    /// <summary>
    /// Attempting to generate a room that is randomly placed in the map. If a room is
    /// created, a room container will be added to containers.
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

        TileAttributes.TileType[] nonGroundTiles = { TileAttributes.TileType.FLOOR, TileAttributes.TileType.WALL };
        // Verify this area is not overlapping non ground tiles.
        bool isRoomPossible = !IsTileTypeInRect(left, top, roomWidth, roomHeight, nonGroundTiles);
        if(isRoomPossible)
        {
            // Since the creation of a room is possible, create a room. The room will be the size of
            // width,height starting from left,top. The room's outter rim will be wallTiles and the
            // room's center will be floorTiles. The room will be contained in a roomContainer.
            GameObject currRoomContainer = InstantiateContainer(roomContainer);
            currRoomContainer.GetComponent<ContainerAttributes>().SetDimensions(left, top, roomWidth, roomHeight);
            SetTileArea(left, top, roomWidth, roomHeight, floorTile, wallTile, currRoomContainer);
        }

        return isRoomPossible;
    }

    private GameObject InstantiateContainer(GameObject container)
    {
        GameObject currContainer = Instantiate(container, gameObject.transform.position, gameObject.transform.rotation);
        currContainer.transform.parent = gameObject.transform;
        containers.Add(currContainer);
        return currContainer;
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

    public GameObject GetTile(int x, int y)
    {
        if (!ValidTileSpace(x, y))
            return null;

        return this.tileMap[y, x];
    }

    /// <summary>
    /// Applies a GameObject to a coordinate in the tileMap as well as adding that tile
    /// to the Unity heirarchy as a child to a Container. Removes the tile that existed 
    /// in the specified location if a tile exists.
    /// </summary>
    /// <returns>
    /// True if the tile was placed correctly, false otherwise (such as when the tile
    /// cannot be placed in the specified coordinate due to the coordinate being invalid)
    /// </returns>
    private bool SetTile(int x, int y, GameObject tile, GameObject container = null)
    {
        // Defaulting container to be gameObject; this has to be done this way as gameObject
        // is a compile time object.
        if (container == null)
            container = gameObject;

        if (!ValidTileSpace(x, y))
            return false;

        // Remove a tile from the Unity hierarchy if one is existing in this tile location.
        GameObject preExistingTile = GetTile(x, y);
        if (preExistingTile)
            Destroy(preExistingTile);

        // Instantiate the tile; this takes a Unity prefab (the tile) and generates a 
        // GameObject from the prefab. The generated GameObject is considered to be a
        // clone of the prefab.
        GameObject tileClone = Instantiate(tile, new Vector3(x * tileOffset, y * tileOffset, 0.0f), Quaternion.identity);

        // Set tile as a child of a container.
        tileClone.transform.parent = container.transform;

        // Give the tile more info about itself.
        TileAttributes tileCloneAttributes = tileClone.GetComponent<TileAttributes>();
        tileCloneAttributes.SetContainer(container);
        tileCloneAttributes.SetMap(gameObject); // The map is this gameObject.
        tileCloneAttributes.SetCords(x, y);

        // Add the tile to the map's (this gameObject) grid. 
        this.tileMap[y, x] = tileClone;

        return true;

    }

    /// <summary>
    /// Sets an area of the map (from left,top to left+width,top+height) all to a 
    /// specified gameTile.
    /// </summary>
    private void SetTileArea(int left, int top, int width, int height, GameObject fillGameObject, GameObject borderGameObject = null, GameObject container = null)
    {
        // Defaulting container to be gameObject; this has to be done this way as gameObject
        // is a compile time object.
        if (container == null)
            container = gameObject;

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
                    SetTile(x, y, borderGameObject, container);
                }
                // Otherwise, place the default fill GameObject
                else
                {
                    SetTile(x, y, fillGameObject, container);
                }
            }
        }
    }

    /// <summary>
    /// Checks if a coordinate is one that exists within the boundaries of the map/
    /// </summary>
    /// TODO: this belongs in a seperate script as it is not directly tied to the
    ///       concept of GeneratingTiles
    private bool ValidTileSpace(int x, int y)
    {
        return (x >= 0 && y >= 0 && x < width && y < height);
    }

}
