using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SimpleGenAlgo : GenAlgo
{

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

    public override void GenerateMap(Map map)
    {
        BlankRoomObj baseRoom = new BlankRoomObj(0, 0, map.w, map.h, map);
        map.AddRoom(baseRoom);
        map.FillArea(baseRoom.x, baseRoom.y, baseRoom.w, baseRoom.h, TileObj.TileObjType.EarthTileObj, baseRoom);


        for (int roomAttempt = 0; roomAttempt < roomAttempts; roomAttempt++)
        {
            AttemptGenRandomRoom();
        }

        
        for (int pathAttempt = 0; pathAttempt < pathAttempts; pathAttempt++)
        {
            AttemptGenRandomPath();
        }

        //Determine a HideoutRoom to put the player in
        HideoutRoomObj playerRoom = map.GetHideoutRoom();
        playerRoom.SpawnPlayer ();
    }

    /// TODO: this is an old description of a very similar function, should be rewritten
    ///
    /// <summary>
    /// Attempting to generate a room that is randomly placed in the map. If a room is
    /// created, a room container will be added to containers.
    /// </summary>
    /// <returns>
    /// False when a room is not created, True otherwise
    /// </returns>
    private bool AttemptGenRandomRoom()
    {
        // Gen a random room. If you do not see your room appearing, look into RoomObj.InstantiateRoomObj()
        RoomObj roomObj = map.GenRandomRoom();

        // Must check if we can place this new room into the map.
        if (!map.IsRoomAreaValid(
                roomObj,

                // If the area the room inhabits includes any of these tiles, do not place the room.
                new TileObj.TileObjType[] {
                    TileObj.TileObjType.FloorTileObj,
                    TileObj.TileObjType.WallTileObj,
                    TileObj.TileObjType.SandTileObj
                })
        )
        {
            // If we cant, forget about adding the room and remove it.
            roomObj.Remove();
            return false;
        }

        // If we can, add the room to the map as well as generating the tiles and placing the
        // tiles on the map.
        map.AddRoom(roomObj);
        roomObj.GenRoom();

        return true;
    }

    private void AttemptGenRandomPath()
    {
        PathRoomObj pathRoomObj = new PathRoomObj(map);
        map.AddRoom(pathRoomObj);

        Utilities.Direction startingDirection = Utilities.RandomEnumValue<Utilities.Direction>();

        RoomObj startingRoom = map.GetRanRoom(new RoomObj.RoomObjType[] { RoomObj.RoomObjType.Blank, RoomObj.RoomObjType.Path });

        TileObj startingTile = null;

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

        AttemptGenRandomPathStep(startingTile, startingDirection, pathRoomObj);
    }

    private void AttemptGenRandomPathStep(TileObj tile, Utilities.Direction direction, PathRoomObj pathRoomObj)
    {
        if (tile == null ||
            tile.tileObjType == TileObj.TileObjType.FloorTileObj ||
            tile.tileObjType == TileObj.TileObjType.SandTileObj)
            return;

        tile = new FloorTileObj(tile.x, tile.y, map);

        map.SetTile(tile.x, tile.y, tile, pathRoomObj);

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
        AttemptGenRandomPathStep(tile.GetNeighbor(direction), direction, pathRoomObj);

    }


}
