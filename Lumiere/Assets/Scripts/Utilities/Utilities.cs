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

    public enum Turn
    {
        FORWARD,
        BACKWARD,
        LEFT,
        RIGHT
    }

    public static Turn GetTurn(Direction currDir, Direction nextDir)
    {
        if (currDir == nextDir) return Turn.FORWARD;

        if (LeftOf(currDir) == nextDir) return Turn.LEFT;

        if (RightOf(currDir) == nextDir) return Turn.RIGHT;

        if (Behind(currDir) == nextDir) return Turn.BACKWARD;

        return Turn.FORWARD;
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

    public static Direction Behind(Direction direction)
    {
        switch (direction)
        {
            case Direction.NORTH:
                return Direction.SOUTH;
            case Direction.EAST:
                return Direction.WEST;
            case Direction.SOUTH:
                return Direction.NORTH;
            case Direction.WEST:
                return Direction.EAST;
        }

        return Direction.NORTH;
    }

    public static Pair<int,int> CordInDirection(Direction direction, int x, int y)
    {
        switch (direction)
        {
            case Direction.NORTH:
                return new Pair<int, int>(x, y - 1);
            case Direction.EAST:
                return new Pair<int, int>(x + 1,y);
            case Direction.SOUTH:
                return new Pair<int, int>(x, y + 1);
            case Direction.WEST:
                return new Pair<int, int>(x - 1, y);
        }

        return new Pair<int, int>(x, y);
    }

    public static Pair<int, int> CordInDirection(Direction direction, Pair<int,int> pair)
    {
        return CordInDirection(direction, pair.First, pair.Second);
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