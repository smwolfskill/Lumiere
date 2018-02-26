using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class SimpleGenAlgo : GenAlgo
{

    private int w, h;
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
        BlankRoomObj baseRoom = new BlankRoomObj(0, 0, 0, 0, map);
        map.AddRoom(baseRoom);

        map.FillArea(baseRoom.x, baseRoom.y, baseRoom.w, baseRoom.h, TileObj.TileObjType.EarthTileObj, baseRoom);

        //EarthTileObj eto = new EarthTileObj(new Vector2Int(1,2), map);
       // map.SetTile(new Vector2Int(1, 1), eto, baseRoom);

       /* for (int roomAttempt = 0; roomAttempt < roomAttempts; roomAttempt++)
        {
            AttemptGenRandomRoom();
        }

        for (int pathAttempt = 0; pathAttempt < pathAttempts; pathAttempt++)
        {
            AttemptGenRandomPath();
        }
        */
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

        return true;
    }



}
