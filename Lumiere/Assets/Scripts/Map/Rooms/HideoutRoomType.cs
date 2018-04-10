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
        int x = room.x;
        int y = room.y;
        int w = room.w;
        int h = room.h;
        map.FillArea(x, y, w, h, floorTile, room);
        map.FillLine(x, y, w, Utilities.Direction.EAST, wallTile, room);
        map.FillLine(x, y, h, Utilities.Direction.SOUTH, wallTile, room);
        map.FillLine(x, y+h-1, w, Utilities.Direction.EAST, wallTile, room);
        map.FillLine(x+w-1, y, h, Utilities.Direction.SOUTH, wallTile, room);
        map.SetTile(x + w - 1, y + h - 1, new Tile(x+w - 1, y+h - 1, map, wallTile), room);
    }
}
