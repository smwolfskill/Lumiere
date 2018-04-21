using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Door
{
    public int x, y, radius;
    public Utilities.Direction direction;
    public Container container;

    public Door(int x, int y, Utilities.Direction direction, Container container, int radius)
    {
        this.x = x;
        this.y = y;

        this.direction = direction;

        this.container = container;

        this.radius = radius;
    }

    public void Destroy()
    {
        container.doors.Remove(this);
    }

}
