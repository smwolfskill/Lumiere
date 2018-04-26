using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System;

[CreateAssetMenu(menuName = "Lumiere/Room/Secret")]
public class SecretRoomType : RoomType
{
    public TileType sandTile;
    public TileType wallTile;
    public float itemPadding = 1.5f;         //dist from secret room walls
    public EntityDropGen secretRoomDropGen;  //holds parameters and methods for dynamic loot

    public override void GenRoom(Room room, Map map)
    {
        map.FillAreaWithBorder(room.x, room.y, room.w, room.h, sandTile, wallTile, room);

        //Generate loot in secret room
        GameItem[] loot = secretRoomDropGen.GenerateLoot(map);
        if(loot.Length == 0)
        {
            return;
        }
        if(loot.Length == 1)
        {
            //Spawn in center of room
            loot[0].CreateGameObject(new Vector3(room.centerX, room.centerY, 0));
            //Debug.Log("Spawned 1 item in center of secret room");
        }
        else //use multiples of 4 scaling
        {
            int dim = (int) Mathf.Floor((loot.Length + 3) / 4) + 1; //dimension of square used to place items on border of
            float usableRoomWidth = room.w - 1.5f * itemPadding;
            float usableRoomHeight = room.h - 1.5f * itemPadding;
            float itemSpacing_x = usableRoomWidth / dim;
            float itemSpacing_y = usableRoomHeight / dim;

            int i = 0;
            for(int y = 0; y < dim; y++)
            {
                if(i == loot.Length)
                {
                    break;
                }
                for(int x = 0; x < dim; x++)
                {
                    if(i == loot.Length)
                    {
                        break;
                    }
                    if(x == 0 || x == (dim - 1) || y == 0 || y == (dim - 1)) //along edge of square: spawn item
                    {
                        //(avoid spawning items in middle of square unless only 1 item)
                        float itemX = room.x + itemPadding + x*itemSpacing_x;
                        float itemY = room.y + itemPadding + y*itemSpacing_y;
                        loot[i].CreateGameObject(new Vector3(itemX, itemY, 0.0f));
                        i++;
                    }
                }
            }
        }
    }

}
