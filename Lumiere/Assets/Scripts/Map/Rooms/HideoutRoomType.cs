using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Room/Hideout")]
public class HideoutRoomType : RoomType
{

    public TileType floorTile;
    public TileType wallTile;

    public override void GenRoom(Room room, Map map)
    {
        map.FillAreaWithBorder(room.x, room.y, room.w, room.h, floorTile, wallTile, room);
    }
}
