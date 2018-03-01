using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Room/Entity Room")]
public class EntityRoomType : RoomType 
{
    public Entity[] entities;
    public int minWidth;
    public int maxWidth;
    public int minHeight;
    public int maxHeight;
    public int minimumEntities;
    public int maximumEntities;
}
