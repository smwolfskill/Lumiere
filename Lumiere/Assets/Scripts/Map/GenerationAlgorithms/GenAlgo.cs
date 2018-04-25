using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class GenAlgo : ScriptableObject
{
    [HideInInspector]
    public Map map;

    public abstract void GenerateMap(Map map, GameObject ExitDoor);

}
