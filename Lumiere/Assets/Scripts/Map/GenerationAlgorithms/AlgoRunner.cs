using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AlgoRunner : MonoBehaviour {

    public GenAlgo genAlgo;

    [Header("Map Properties")]
    public int width = 50;
    public int height = 50;
    public int tileOffset = 5;

    [Header("Room Properties")]
    public RoomProperties roomProperties;

    private Map map;

    void Start ()
    {

        map = new Map(width, height, tileOffset, gameObject, roomProperties);
        genAlgo.GenerateMap(map);
        
    }

}
