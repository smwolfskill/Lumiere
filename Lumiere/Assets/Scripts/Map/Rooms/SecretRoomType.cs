using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Room/Secret")]
public class SecretRoomType : RoomType
{
    public TileType sandTile;
    public TileType wallTile;
    
    public override void GenRoom(Room room, Map map)
    {
        map.FillAreaWithBorder(room.x, room.y, room.w, room.h, sandTile, wallTile, room);

    }

}
