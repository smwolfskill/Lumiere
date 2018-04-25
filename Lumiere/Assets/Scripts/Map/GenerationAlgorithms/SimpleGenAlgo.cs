using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Generation Algorithms/Simple Generation Algorithm")]
public class SimpleGenAlgo : GenAlgo
{

    public int roomAttempts;
    public int pathAttempts;
    public int pathDirectionChangeLikelihood;
    public TileType[] walkableTileTypes;
    public TileType earthTileType;
    public TileType floorTileType;
    public RoomType blankRoomType;
    public RoomType pathRoomType;

    public override void GenerateMap(Map map, GameObject ExitDoor)
    {
        //Needs refactoring before can be functional again.
        /*
        Room baseRoom = new Room(map, 0, 0, map.w, map.h, blankRoomType);
        map.AddContainer(baseRoom);
        map.FillArea(baseRoom.x, baseRoom.y, baseRoom.w, baseRoom.h, earthTileType, baseRoom);


        for (int roomAttempt = 0; roomAttempt < roomAttempts; roomAttempt++)
        {
            AttemptGenRandomRoom(map);
        }

        
        for (int pathAttempt = 0; pathAttempt < pathAttempts; pathAttempt++)
        {
            AttemptGenRandomPath(map);
        }

        //Determine a HideoutRoom to put the player in
        Room playerRoom = map.GetRanContainer(new RoomType[] { blankRoomType, pathRoomType });

        if (playerRoom == null)
        {
            Debug.Log("A player could not be spawned due to SimpleGenAlgo not generating" +
                "a map that has a valid room to spawn a player in");
        }
        else
        {
            playerRoom.SpawnPlayer();
        }
        */
    }

    /*
    /// TODO: this is an old description of a very similar function, should be rewritten
    ///
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

    private void AttemptGenRandomPath(Map map)
    {
        Room pathRoom = map.GenRoom (pathRoomType);
        map.AddContainer(pathRoom);

        Utilities.Direction startingDirection = Utilities.RandomEnumValue<Utilities.Direction>();
        Room startingRoom = map.GetRanContainer(new RoomType[] { blankRoomType, pathRoomType });

        if(startingRoom.w < 3 || startingRoom.h < 3)
        {
            return;
        }

        Tile startingTile = null;

        switch (startingDirection)
        {
            case Utilities.Direction.NORTH:

                // We want to choose a tile that is not in a corner and that is on the
                // north wall of the room.

                startingTile = map.GetTile(
                    Utilities.RandomIntInRange(startingRoom.x + 1, startingRoom.x + startingRoom.w - 1),
                    startingRoom.y
                );
                break;

            case Utilities.Direction.SOUTH:
                startingTile = map.GetTile(
                    Utilities.RandomIntInRange(startingRoom.x + 1, startingRoom.x + startingRoom.w - 1),
                    startingRoom.y + startingRoom.h - 1
                );
                break;

            case Utilities.Direction.WEST:
                startingTile = map.GetTile(
                    startingRoom.x,
                    Utilities.RandomIntInRange(startingRoom.y + 1, startingRoom.y + startingRoom.h - 1)
                );
                break;

            case Utilities.Direction.EAST:
                startingTile = map.GetTile(
                    startingRoom.x + startingRoom.w - 1,
                    Utilities.RandomIntInRange(startingRoom.y + 1, startingRoom.y + startingRoom.h- 1)
                );
                break;
        }

        AttemptGenRandomPathStep(map, startingTile, startingDirection, pathRoom);
    }

    private void AttemptGenRandomPathStep(Map map, Tile tile, Utilities.Direction direction, Room pathRoom)
    {
        if (tile == null || tile.IsWalkable())
            return;

        tile = new Tile(tile.x, tile.y, map, floorTileType);

        map.SetTile(tile.x, tile.y, tile, pathRoom);

        int directionChangeVal = Utilities.RandomIntInRange(0, pathDirectionChangeLikelihood);
        if (directionChangeVal == 0)
        {
            direction = Utilities.LeftOf(direction);
        }
        else if (directionChangeVal == 1)
        {
            direction = Utilities.RightOf(direction);
        }

        // Take another step in creating the path.
        AttemptGenRandomPathStep(map, tile.GetNeighbor(direction), direction, pathRoom);

    }

    */


}
