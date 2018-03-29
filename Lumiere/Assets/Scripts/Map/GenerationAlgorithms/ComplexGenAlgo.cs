using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Generation Algorithms/Complex Generation Algorithm")]
public class ComplexGenAlgo : GenAlgo
{

    public int roomAttempts;
    public int minDoorsPerRoom;
    public int maxDoorsPerRoom;
    public TileType[] walkableTileTypes;
    public TileType earthTileType;
    public ContainerType baseContainerType;

    public override void GenerateMap(Map map)
    {
        Container baseContainer = new Container(map, baseContainerType);
        map.AddContainer(baseContainer);
        map.FillArea(0, 0, map.w, map.h, earthTileType, baseContainer); 

        for (int roomAttempt = 0; roomAttempt < roomAttempts; roomAttempt++)
        {
            AttemptGenRandomRoom(map);
        }

        
    }

    /// <summary>
    /// Attempting to generate a room that is randomly placed in the map. If a room is
    /// created, a room container will be added to containers.
    /// </summary>
    /// <returns>
    /// False when a room is not created, True otherwise
    /// </returns>
    private bool AttemptGenRandomRoom(Map map)
    {
        // Gen a random room. If you do not see your room appearing, look into RoomObj.InstantiateRoomObj()
        Room room = map.GenRandomRoom();

        // Must check if we can place this new room into the map.
        if (!map.IsRoomAreaValid(room, walkableTileTypes))
        {
            // If we cant, forget about adding the room and remove it.
            room.Remove();
            return false;
        }

        // If we can, add the room to the map as well as generating the tiles and placing the
        // tiles on the map.
        map.AddContainer(room);
        room.GenRoom();

        return true;
    }
    


}
