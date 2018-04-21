using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Generation Algorithms/Complex Generation Algorithm")]
public class ComplexGenAlgo : GenAlgo
{

    public int roomAttempts;
    public int spaceBetweenRooms = 5;
    public TileType[] walkableTileTypes;
    public TileType earthTileType;
    public TileType pathTileType;
    public TileType wallTileType;
    public ContainerType baseContainerType;
    public bool longButGoodGen = false;

    public override void GenerateMap(Map map)
    {
        this.map = map;

        Container baseContainer = new Container(map, baseContainerType);
        map.AddContainer(baseContainer);
        map.FillArea(0, 0, map.w, map.h, earthTileType, baseContainer); 

        for (int roomAttempt = 0; roomAttempt < roomAttempts; roomAttempt++)
        {
            AttemptGenRandomRoom(map);
        }

        map.PopulateClosestOtherRooms();

        
        AttemptGenPaths();

        if(longButGoodGen)DesperateConnect();

        map.GetRooms()[0].SpawnPlayer();
    }

    private void DesperateConnect()
    {

        for(int i = map.containers.Count - 1; i >= 0; i--)
        {
            Container startingContainer = map.containers[i];

            for(int j = i - 1; j >= 0; j--)
            {
                Container endingContainer = map.containers[j];

                if (!map.AreContainersConnected(startingContainer, endingContainer))
                {
                    TryConnect(startingContainer, endingContainer);
                }
            }
        }

    }

    private bool TryConnect(Container startingContainer, Container endingContainer)
    {
        List<Tile> endingTiles = endingContainer.GetTilesOfType(wallTileType);
        List<Tile> startingTiles = startingContainer.GetTilesOfType(wallTileType);

        foreach(Tile startingTile in startingTiles)
        {
            Door startingDoor = GetValidDoor(startingTile);
            if (startingDoor == null) continue;

            foreach (Tile endingTile in endingTiles)
            {
                Door endingDoor = GetValidDoor(endingTile);
                if (endingDoor == null) continue;

                if (ConnectDoors(startingDoor, endingDoor))
                {
                    map.ChangeTilesInArea(startingTile.x, startingTile.y, 2, earthTileType, wallTileType, startingContainer);
                    map.ChangeTilesInArea(startingTile.x, startingTile.y, 1, wallTileType, pathTileType, startingContainer);

                    map.ChangeTilesInArea(endingTile.x, endingTile.y, 2, earthTileType, wallTileType, endingContainer);
                    map.ChangeTilesInArea(endingTile.x, endingTile.y, 1, wallTileType, pathTileType, endingContainer);

                    return true;
                }
            }
        }

        return false;

    }

    // Given a tile, get a valid door for this tile
    private Door GetValidDoor(Tile tile)
    {
        foreach (Utilities.Direction direction in Utilities.Direction.GetValues(typeof(Utilities.Direction)))
        {
            Pair<int,int> pair = Utilities.CordInDirection(direction, tile.x, tile.y);
            Tile attemptDoorDirectionTile = map.GetTile(pair.First, pair.Second);
            if(attemptDoorDirectionTile != null && attemptDoorDirectionTile.tileType == earthTileType)
            {
                return new Door(tile.x, tile.y, direction, tile.container, 1);
            }
        }

        return null;
    }


    private void AttemptGenPaths()
    {
        // connect all doors for all rooms
        for(int i = map.GetRooms().Count - 1; i >= 0; i--)
        {
            Room room = map.GetRooms()[i];

            for (int j = room.doors.Count - 1; j >= 0; j--)
            {
                Door door = room.doors[j];

                bool nextDoor = false;

                // try connecting to the cloest otherRooms first
                foreach (Room otherRoom in room.closestOtherRooms)
                {
                    foreach(Door otherDoor in otherRoom.doors)
                    {
                        // On a successful door connection, stop trying to find a path for this
                        // door. This means that not only should we stop looking at all otherDoors
                        // for this otherRoom, but we should stop looking at all otherRooms for this
                        // door to connect to.
                        if (ConnectDoors(door, otherDoor)) nextDoor = true;

                        if (nextDoor) break;
                    }

                    if (nextDoor) break;
                }

            }
        }
    }

    // return true on a successful connection and modify the map. on an unsuccessful
    // conection, return false and do not modify the map
    private bool ConnectDoors(Door door, Door otherDoor)
    {

        Pair<int,int> doorPushedPair = Utilities.CordInDirection(door.direction, door.x, door.y);
        Pair<int, int> otherDoorPushedPair = Utilities.CordInDirection(otherDoor.direction, otherDoor.x, otherDoor.y);

        // create psudo doors that are pushed away from Rooms
        Door doorPushed = new Door(doorPushedPair.First, doorPushedPair.Second, door.direction, door.container, door.radius);
        Door otherDoorPushed = new Door(otherDoorPushedPair.First, otherDoorPushedPair.Second, otherDoor.direction, otherDoor.container, otherDoor.radius);

        //create opening for path to find
        map.CreateTileAndSetTile(otherDoor.x, otherDoor.y, otherDoor.container, earthTileType);

        // The pair contains the current tile and the parent tile
        Queue<Link> queue = new Queue<Link>();

        Tile currTile = map.GetTile(doorPushed.x, doorPushed.y);

        // Given a tile, return true if this tile has been searched, otherwise false. This
        // dict is modified in AddToQueue() as once a path has been added, it has been
        // searched.
        Dictionary<Tile, bool> tileHasBeenSearched = new Dictionary<Tile, bool>();

        Link currLink = AddToQueue(currTile, null, queue, tileHasBeenSearched, doorPushed.direction);

        Link endLink = null;

        while(endLink == null && queue.Count != 0)
        {                                    
            endLink = BFSStep(queue, tileHasBeenSearched, otherDoorPushed);
        }

        // close up opening whether path found it or not
        map.CreateTileAndSetTile(otherDoor.x, otherDoor.y, otherDoor.container, wallTileType);

        // successful path found
        if (endLink != null)
        {
            //add the path to the map
            Container path = ModifyMapWithLinks(endLink);

            map.ConnectContainers(door.container, path);
            map.ConnectContainers(otherDoor.container, path);


            //add openings to the door areas
            map.CreateTileAndSetTile(door.x, door.y, door.container, pathTileType);
            map.CreateTileAndSetTile(otherDoor.x, otherDoor.y, otherDoor.container, pathTileType);

            // never use the doors again
            door.Destroy();
            otherDoor.Destroy();

            return true;
        }


        return false;
    }


    private Link BFSStep(Queue<Link> queue, Dictionary<Tile, bool> tileHasBeenSearched, Door otherDoor)
    {
        Link link = queue.Dequeue();

        foreach(Utilities.Direction direction in Utilities.Direction.GetValues(typeof(Utilities.Direction)))
        {
            if (!CanTakeThisDirection(link, direction)) continue;

            Link newLink = AddToQueue(link, direction, queue, tileHasBeenSearched);

            // Check to see if the goal has been reached
            if(newLink != null && newLink.currTile.x == otherDoor.x && newLink.currTile.y == otherDoor.y)
            {
                return newLink;
            }
        }

        return null;
    }

    private bool CanTakeThisDirection(Link link, Utilities.Direction direction)
    {
        return true;
        List<Utilities.Turn> turnsToCheck = new List<Utilities.Turn>();

        switch(Utilities.GetTurn(link.currTileDirection, direction))
        {
            case Utilities.Turn.FORWARD:
                return true;

            case Utilities.Turn.LEFT:
                turnsToCheck.Add(Utilities.Turn.FORWARD);


        }
    }

    private Container ModifyMapWithLinks(Link currLink)
    {
        // todo: choose a more specific container type for the path's container
        Container container = new Container(map, baseContainerType);
        map.AddContainer(container);

        while (currLink != null)
        {
            Tile tile = new Tile(currLink.currTile.x, currLink.currTile.y, map, pathTileType);
            map.SetTile(currLink.currTile.x, currLink.currTile.y, tile, container);

            map.ChangeTilesInArea(currLink.currTile.x, currLink.currTile.y, 1, earthTileType, wallTileType, container);

            currLink = currLink.parentLink;
        }

        return container;

    }

    private Link AddToQueue(
        Tile currTile,
        Link parentLink,
        Queue<Link> queue,
        Dictionary<Tile, bool> tileHasBeenSearched,
        Utilities.Direction direction
    )
    {
        if (!IsValidTile(currTile, tileHasBeenSearched, direction)) return null;

        Link currLink = new Link(currTile, parentLink, direction);

        if (currLink == null) return null;

        queue.Enqueue(currLink);

        SearchTile(tileHasBeenSearched, currTile);

        return currLink;
    }

    private Link AddToQueue(
        Link parentLink,
        Utilities.Direction direction,
        Queue<Link> queue,
        Dictionary<Tile, bool> tileHasBeenSearched
    )
    {
        if (parentLink == null) return null;

        int currX = parentLink.currTile.x;
        int currY = parentLink.currTile.y;

        int newX = 0;
        int newY = 0;

        switch (direction)
        {
            case Utilities.Direction.NORTH:
                newX = currX;
                newY = currY - 1;
                break;
            case Utilities.Direction.SOUTH:
                newX = currX;
                newY = currY + 1;
                break;
            case Utilities.Direction.WEST:
                newX = currX - 1;
                newY = currY;
                break;
            case Utilities.Direction.EAST:
                newX = currX + 1;
                newY = currY;
                break;
        }

        Tile currTile = map.GetTile(newX, newY);

        return AddToQueue(currTile, parentLink, queue, tileHasBeenSearched, direction);
    }

    private bool IsValidTile(Tile potentialTile, Dictionary<Tile, bool> tileHasBeenSearched, Utilities.Direction direction)
    {
        if(
            potentialTile == null ||
            potentialTile.tileType != earthTileType ||
            TileHasBeenSearched(tileHasBeenSearched, potentialTile)
        )
        {
            return false;
        }

        Pair<int, int> currLeftOfPair = new Pair<int, int>(potentialTile.x, potentialTile.y);
        Pair<int, int> currRightOfPair = new Pair<int, int>(potentialTile.x, potentialTile.y);
        Utilities.Direction leftOf = Utilities.LeftOf(direction);
        Utilities.Direction rightOf = Utilities.RightOf(direction);

        for(int i = 0; i < spaceBetweenRooms / 2; i++)
        {
            currLeftOfPair = Utilities.CordInDirection(leftOf, currLeftOfPair);
            currRightOfPair = Utilities.CordInDirection(rightOf, currRightOfPair);

            if
            (
                !IsValidSurroundingTile(currLeftOfPair) ||
                !IsValidSurroundingTile(currRightOfPair)
            )
                return false;
        }

        Pair<int, int> forwardPair = Utilities.CordInDirection(direction, potentialTile.x, potentialTile.y);
        if (!IsValidSurroundingTile(forwardPair)) return false;

        return true;
    }

    private bool IsValidSurroundingTile(int x, int y)
    {
        return IsValidSurroundingTile(new Pair<int, int>(x, y));
    }

    private bool IsValidSurroundingTile(Pair<int, int> pair)
    {
        Tile tile = map.GetTile(pair.First, pair.Second);

        return (tile != null && tile.tileType == earthTileType);
    }

    private bool TileHasBeenSearched(Dictionary<Tile, bool> tileHasBeenSearched, Tile tile)
    {
        // if tileHasBeenSearched has a key and returns true, it means the tile
        // has been searched. otherwise, the tile has not been searched.
        return (tileHasBeenSearched.ContainsKey(tile) && tileHasBeenSearched[tile]);
    }

    private bool TileHasBeenSearched(Dictionary<Tile, bool> tileHasBeenSearched, int x, int y)
    {
        if (map.GetTile(x, y) == null) return true;
        return TileHasBeenSearched(tileHasBeenSearched, map.GetTile(x, y));
    }

    private void SearchTile(Dictionary<Tile, bool> tileHasBeenSearched, Tile tile)
    {
        // the newest addition to path has now been searched.
        tileHasBeenSearched.Add(
            tile,
            true
        );
    }

    // uses end of path as tile to search
    private void SearchTile(Dictionary<Tile, bool> tileHasBeenSearched, List<Pair<int, int>> path)
    {
        SearchTile(tileHasBeenSearched, map.GetTile(Utilities.EndOfList(path).First, Utilities.EndOfList(path).Second));
    }
    



    /// <summary>
    /// Attempting to generate a room that is randomly placed in the map. If a room is
    /// created, a room container will be added to containers.
    /// </summary>
    /// <returns>
    /// False when a room is not created, True otherwise
    /// </returns>
    private void AttemptGenRandomRoom(Map map)
    {
        // Gen a random room. If you do not see your room appearing, look into RoomObj.InstantiateRoomObj()
        Room room = map.GenRandomRoom();

        // Must check if we can place this new room into the map.
        if (!map.DoesAreaContainOnlyThisTile(
            room.x - spaceBetweenRooms,
            room.y - spaceBetweenRooms,
            room.w + spaceBetweenRooms*2,
            room.h + spaceBetweenRooms*2,
            earthTileType
        ))
        {
            // If we cant, forget about adding the room and remove it.
            room.Remove();

            return;
        }

        // If we can, add the room to the map as well as generating the tiles and placing the
        // tiles on the map.
        map.AddContainer(room);
        room.GenRoom(spaceBetweenRooms / 2);
    }
    


}

public class Link
{
    public Tile currTile;
    public Link parentLink;
    public Utilities.Direction currTileDirection;

    public Link(Tile currTile, Link parentLink, Utilities.Direction currTileDirection)
    {
        this.currTile = currTile;
        this.parentLink = parentLink;
        this.currTileDirection = currTileDirection;
    }
}