using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SimpleGenAlgo : GenAlgo
{

    private Vector2Int w_h;
    private int roomAttempts;
    private int pathAttempts;
    private int pathDirectionChangeLikelihood;

    public SimpleGenAlgo(
        Map map,
        int roomAttempts,
        int pathAttempts,
        int pathDirectionChangeLikelihood
    ) : base(map)
    {
        this.roomAttempts = roomAttempts;
        this.pathAttempts = pathAttempts;
        this.pathDirectionChangeLikelihood = pathDirectionChangeLikelihood;
    }

    public override void GenerateMap(GameObject map)
    {

    }


    /*

    //public RoomTypes[] = [sheelBedRoom <- ScriptableObject, wyattBedRoom <- ScriptableObject]

    private GameTile[,] tileMatrix;

    private Room[] rooms;



    void Start ()
    {

        //gen 10 rooms
        //for i in 10:
        //
        //  pick a random scriptable object from roomtypes
        //  
        //  now we have sheelBedRoom
        //
        //  now instantiate sheelBedRoom
        //  gen ran width height from sheelbedroom
        //  
        //  RoomType (the scriptable object) is also used to take Room (basic data structure)
        //  and turns it into a GameObject and puts it into the scene




        // hey, i want a room
        // what kinda room?
        // sheel bed room
        // SheelBedRoom scirptable object tells you how to make that room
        // the room you make is a Room, NOT a SheelBedRoom (bc that's static)
        // how to take Room and make a GameObject?
        // RoomType.GenAndPopulateScene(Room room) <- take a Room and put it into the scene as a gameobject

        
    }
    */
}
