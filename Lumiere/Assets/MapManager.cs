using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MapManager : MonoBehaviour
{

    public int levelNumber;
    public float difficulty;

    public int initalMapSize;
    public float mapSizeIncreaseFactor;


    public ComplexGenAlgo complexGenAlgo;
    public RoomProperties roomProperties;

    private GameObject currMap;

    void Start()
    {
        GenMap(levelNumber, difficulty);
    }


    public void GenMap(int levelNumber, float difficulty)
    {
        currMap = new GameObject();

        int width = (int)(initalMapSize + levelNumber * mapSizeIncreaseFactor);
        int height = (int)(initalMapSize + levelNumber * mapSizeIncreaseFactor);

        complexGenAlgo.roomAttempts = 1000;
        complexGenAlgo.spaceBetweenRooms = 5;


        Map map = new Map(width, height, 1, currMap, roomProperties, levelNumber, difficulty);
        complexGenAlgo.GenerateMap(map);

    }

}
