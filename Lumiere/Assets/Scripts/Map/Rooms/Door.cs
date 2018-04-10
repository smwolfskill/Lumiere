using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Door
{
    public int x, y, radius;
    public Utilities.Direction direction;
    public Room room;

    public Door(int x, int y, Utilities.Direction direction, Room room, int radius)
    {
        this.x = x;
        this.y = y;

        this.direction = direction;

        this.room = room;

        this.radius = radius;
    }

    public void Destroy()
    {
        room.doors.Remove(this);
    }

}
