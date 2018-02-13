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

}
