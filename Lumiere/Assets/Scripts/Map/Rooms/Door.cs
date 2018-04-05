using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Door
{
    public int x, y;
    public Utilities.Direction direction;

    public Door(int x, int y, Utilities.Direction direction)
    {
        this.x = x;
        this.y = y;

        this.direction = direction;
    }


}
