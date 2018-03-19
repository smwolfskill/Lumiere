using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[System.Serializable]
[CreateAssetMenu(menuName = "Lumiere/Room Properties")]
public class RoomProperties : ScriptableObject 
{
    public int minWidth;
    public int minHeight;
    public int maxWidth;
    public int maxHeight;
    public RoomType[] roomTypes;
}
