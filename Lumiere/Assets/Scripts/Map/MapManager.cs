using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MapManager : MonoBehaviour
{

    public bool GenMapOnStart = false;

    private int levelNumber = 1;
    private const float difficulty = 1.5f;

    private const int initalMapSize = 40;
    private const float mapSizeIncreaseFactor = 3;


    public ComplexGenAlgo complexGenAlgo;
    public RoomProperties roomProperties;

    private GameObject currMap;

    public GameObject ExitDoor;

    void Start()
    {
        levelNumber = 1;

        if(GenMapOnStart)
        {
            GenMap();
        }

    }


    public void GenMap()
    {
        currMap = new GameObject();

        int width = (int)(initalMapSize + 1 * mapSizeIncreaseFactor);
        int height = (int)(initalMapSize + 1 * mapSizeIncreaseFactor);

        complexGenAlgo.roomAttempts = 1000;
        complexGenAlgo.spaceBetweenRooms = 5;

        Map map = new Map(width, height, 1, currMap, roomProperties, levelNumber, difficulty);
        complexGenAlgo.GenerateMap(map, ExitDoor);

        levelNumber++;
    }

    public void DestroyMap()
    {
        //Default layer = 0
        //DroppedItems layer = 8
        //Enemy layer = 9

        GameObject[] gameObjectArray = FindObjectsOfType(typeof(GameObject)) as GameObject[];
        foreach(GameObject gameObject in gameObjectArray)
        {
            if(gameObject.layer == 8 || gameObject.layer == 0 || gameObject.layer == 9)
            {
                Destroy(gameObject);
            }
        }
    }
}
