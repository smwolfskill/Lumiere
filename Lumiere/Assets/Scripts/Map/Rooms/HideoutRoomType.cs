using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Room/Hideout")]
public class HideoutRoomType : RoomType
{
    public Player player;
    public int minWidth;
    public int maxWidth;
    public int minHeight;
    public int maxHeight;
}
