using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class RoomType : BaseObject
{
    public Player player;

    virtual public GameObject PopulateGameObject()
    {
        return new GameObject(name);
    }

    public abstract void GenRoom(Room room, Map map);
}
