using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class TileAttributes : MonoBehaviour {

    public enum TileType
    {
        EARTH,
        WALL,
        FLOOR
    };

    public TileType tileType = TileType.EARTH;

    private GameObject map;
    private GameObject container;

    // On the grid, not positionally in unity.
    public int x;
    public int y;

    public void SetCords(int x, int y)
    {
        this.x = x;
        this.y = y;
    }
    public int GetX()
    {
        return this.x;
    }
    public int GetY()
    {
        return this.y;
    }

    public void SetMap(GameObject map)
    {
        this.map = map;
    }

    public void SetContainer(GameObject container)
    {
        this.container = container;
    }

    // Refactor suggestion: Adding a function like this makes the name TileAttributes not as applicable anymore.
    public GameObject GetNeighbor(Utilities.Direction direction)
    {
        switch (direction)
        {
            case Utilities.Direction.NORTH:
                return map.GetComponent<GenerateTiles>().GetTile(x, y - 1);

            case Utilities.Direction.SOUTH:
                return map.GetComponent<GenerateTiles>().GetTile(x, y + 1);

            case Utilities.Direction.WEST:
                return map.GetComponent<GenerateTiles>().GetTile(x - 1, y);

            case Utilities.Direction.EAST:
                return map.GetComponent<GenerateTiles>().GetTile(x + 1, y);

        }

        return null;
    }
}
