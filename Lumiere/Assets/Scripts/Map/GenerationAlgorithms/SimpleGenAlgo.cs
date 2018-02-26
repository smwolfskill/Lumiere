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

    public override void GenerateMap(Map map)
    {
        BlankRoomObj baseRoom = new BlankRoomObj(new Vector2Int(0, 0), w_h, map);
        map.AddRoom(baseRoom);

        map.FillArea(baseRoom.x_y, baseRoom.w_h, TileObj.TileObjType.EarthTileObj, baseRoom);

        for (int roomAttempt = 0; roomAttempt < roomAttempts; roomAttempt++)
        {
            AttemptGenRandomRoom();
        }

        for (int pathAttempt = 0; pathAttempt < pathAttempts; pathAttempt++)
        {
            AttemptGenRandomPath();
        }

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
        int roomWidth = random.Next(smallestRoomDim, largestRoomDim + 1);
        int roomHeight = random.Next(smallestRoomDim, largestRoomDim + 1);

        int left = random.Next(0, this.width - roomWidth);
        int top = random.Next(0, this.height - roomHeight);

        TileAttributes.TileType[] nonGroundTiles = { TileAttributes.TileType.FLOOR, TileAttributes.TileType.WALL };
        // Verify this area is not overlapping non ground tiles.
        bool isRoomPossible = !IsTileTypeInRect(left, top, roomWidth, roomHeight, nonGroundTiles);
        if (isRoomPossible)
        {
            // Since the creation of a room is possible, create a room. The room will be the size of
            // width,height starting from left,top. The room's outter rim will be wallTiles and the
            // room's center will be floorTiles. The room will be contained in a roomContainer.
            GameObject currRoomContainer = InstantiateContainer(roomContainer);
            currRoomContainer.GetComponent<ContainerAttributes>().SetDimensions(left, top, roomWidth, roomHeight);
            SetTileArea(left, top, roomWidth, roomHeight, floorTile, wallTile, currRoomContainer);
        }

        return isRoomPossible;
    }



}
