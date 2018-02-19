using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ContainerAttributes
{
    public enum ContainerType
    {
        ROOM,
        EARTH,
        PATH
    }

    public ContainerType containerType = ContainerType.EARTH;

    // These do not apply to all container types, so a good refactor would be
    // to put them in a seperate script that only applies to the classes they
    // belong to. For instance, these do not belong to PathContainers
    private int top;
    private int left;
    private int width;
    private int height;

    public void SetDimensions(int left, int top, int width, int height)
    {
        this.top = top;
        this.left = left;
        this.width = width;
        this.height = height;
    }

    public int GetTop()
    {
        return top;
    }
    public int GetLeft()
    {
        return left;
    }
    public int GetWidth()
    {
        return width;
    }
    public int GetHeight()
    {
        return height;
    }

}
