using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ScriptRunner : MonoBehaviour {

    public int mapWidth = 50;
    public int mapHeight = 50;

	// Use this for initialization
	void Start () {

        new GameMap(mapWidth, mapHeight);

	}
	
	// Update is called once per frame
	void Update () {
		
	}
}
