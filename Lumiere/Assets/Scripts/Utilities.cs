using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Utilities : MonoBehaviour
{
    public enum Direction
    {
        NORTH,
        SOUTH,
        EAST,
        WEST
    }

    public static Direction LeftOf(Direction direction)
    {
        switch(direction)
        {
            case Direction.NORTH:
                return Direction.WEST;
            case Direction.WEST:
                return Direction.SOUTH;
            case Direction.SOUTH:
                return Direction.EAST;
            case Direction.EAST:
                return Direction.NORTH;
        }

        return Direction.NORTH;
    }

    public static Direction RightOf(Direction direction)
    {
        switch (direction)
        {
            case Direction.NORTH:
                return Direction.EAST;
            case Direction.EAST:
                return Direction.SOUTH;
            case Direction.SOUTH:
                return Direction.WEST;
            case Direction.WEST:
                return Direction.NORTH;
        }

        return Direction.NORTH;
    }
}
