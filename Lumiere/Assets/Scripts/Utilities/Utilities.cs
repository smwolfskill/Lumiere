using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Utilities : MonoBehaviour
{
    public static System.Random random = new System.Random();

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

    public static int RandomIntInRange(int minInclusive, int maxExclusive)
    {
        return random.Next(minInclusive, maxExclusive);
    }

    public static T RandomEnumValue<T>()
    {
        var v = System.Enum.GetValues(typeof(T));
        return (T)v.GetValue(new System.Random().Next(v.Length));
    }


}

public class Pair<T, U>
{
    public Pair(T first, U second)
    {
        this.First = first;
        this.Second = second;
    }

    public T First { get; set; }
    public U Second { get; set; }
};