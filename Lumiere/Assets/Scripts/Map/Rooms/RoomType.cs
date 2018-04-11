using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class RoomType : ContainerType
{
    public Player player;

    public abstract void GenRoom(Room room, Map map);
}
