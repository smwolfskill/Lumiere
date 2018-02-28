using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Room/Entity Room")]
public class EntityRoomType : RoomType 
{
    public Entity[] entities;
    public int minimumEntities;
    public int maximumEntities;
}
