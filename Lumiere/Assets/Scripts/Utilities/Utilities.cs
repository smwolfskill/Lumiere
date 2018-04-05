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
        return (T)v.GetValue(random.Next(v.Length));
    }

    public static T EndOfList<T>(List<T> list)
    {
        return list[list.Count - 1];
    }

    public static void RemoveEndOfList<T>(List<T> list)
    {
        list.RemoveAt(list.Count - 1);
    }

    public static float GetAngle(int currX, int currY, int destX, int destY)
    {
        int difX = destX - currX;
        int difY = destY - currY;
        float angle =  Mathf.Atan2(difY, difX) * (180.0f / Mathf.PI);

        if (angle < 0) angle += 360;
        return angle;
    }

}

public class Pair<T, U>
{
    public Pair(T first, U second)
    {
        this.First = first;
        this.Second = second;
    }

    public T First
    {
        get;
        set;
    }
    public U Second
    {
        get;
        set;
    }
};