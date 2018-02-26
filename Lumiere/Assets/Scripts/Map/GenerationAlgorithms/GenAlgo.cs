using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class GenAlgo
{
    private System.Random random;

    private Map map;

    public GenAlgo(Map map)
    {
        this.map = map;

        random = new System.Random();
    }

    public abstract void GenerateMap(GameObject map);

}
