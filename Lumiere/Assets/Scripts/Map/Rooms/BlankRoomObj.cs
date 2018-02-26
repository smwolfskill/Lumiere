using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class BlankRoomObj : RoomObj
{
    public BlankRoomObj(Vector2Int x_y, Vector2Int w_h, Map map) : base(map)
    {
        this.x_y = x_y;
        this.w_h = w_h;
    }
}
