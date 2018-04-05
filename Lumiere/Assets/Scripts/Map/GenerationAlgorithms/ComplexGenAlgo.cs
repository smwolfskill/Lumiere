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
    public ContainerType baseContainerType;

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
        
    }

    private void AttemptGenPaths()
    {
        // connect all doors for all rooms
        foreach(Room room in map.GetRooms())
        {
            foreach(Door door in room.doors)
            {
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

        // TODO: REMOVE - FOR TESTING ONLY
        Container baseContainer = new Container(map, baseContainerType);
        map.AddContainer(baseContainer);
        map.SetTile(otherDoor.x, otherDoor.y,
            new Tile(otherDoor.x, otherDoor.y, map, earthTileType),
            baseContainer);
        map.SetTile(door.x, door.y,
            new Tile(door.x, door.y, map, earthTileType),
            baseContainer);

        // The pair contains the current tile and the parent tile
        Queue<Link> queue = new Queue<Link>();

        Tile currTile = map.GetTile(door.x, door.y);

        // Given a tile, return true if this tile has been searched, otherwise false. This
        // dict is modified in AddToQueue() as once a path has been added, it has been
        // searched.
        Dictionary<Tile, bool> tileHasBeenSearched = new Dictionary<Tile, bool>();

        Link currLink = AddToQueue(currTile, null, queue, tileHasBeenSearched);

        int halfWayInbetweenRooms = (int)Mathf.Ceil(((float)spaceBetweenRooms) / 2.0f);

        for (int i = 0; i < halfWayInbetweenRooms; i++)
        {
            currLink = AddToQueue(currLink, door.direction, queue, tileHasBeenSearched);
        }

        Link endLink = null;

        while(endLink == null && queue.Count != 0)
        {
            endLink = BFSStep(queue, tileHasBeenSearched, otherDoor);
        }

        if(endLink != null)
        {
            ModifyMapWithLinks(endLink);
        }

        return true;
        /*

        List<Pair<int, int>> path = new List<Pair<int,int>>();


        // Given a tile, return true if this tile has been searched, otherwise false. This
        // dict is modified in AddToPath() as once a path has been added, it has been
        // searched.
        Dictionary<Tile, bool> tileHasBeenSearched = new Dictionary<Tile, bool>();

        // Add the starting tile, the door, to the path. This is necessary to
        // use AddToPath as a single tile needs to exist in path
        path.Add(new Pair<int, int>(door.x, door.y));

        // Since we added to the path, we need to search that tile. This is done automatically
        // in AddToPath()
        SearchTile(tileHasBeenSearched, path);

        // Add the first few tiles to the path. These are the tiles that leave
        // the door and go outside of the room.
        for (int i = 0; i < halfWayInbetweenRooms; i++)
        {
            AddToPath(path, door.direction, tileHasBeenSearched);
        }

        // If the BFS is successful (finds the otherDoor), then use the
        // populated path to draw a path on the map
        if (BFSStep(path, tileHasBeenSearched, otherDoor))
        {
            ModifyMapUsingPath(path);
        }

        return true;

        */
    }

    private Link BFSStep(Queue<Link> queue, Dictionary<Tile, bool> tileHasBeenSearched, Door otherDoor)
    {
        Link link = queue.Dequeue();

        foreach(Utilities.Direction direction in Utilities.Direction.GetValues(typeof(Utilities.Direction)))
        {
            Link newLink = AddToQueue(link, direction, queue, tileHasBeenSearched);

            if(newLink != null && newLink.currTile.x == otherDoor.x && newLink.currTile.y == otherDoor.y)
            {
                return link;
            }
        }

        return null;
    }

    private void ModifyMapWithLinks(Link currLink)
    {
        // todo: choose a more specific container type for the path's container
        Container container = new Container(map, baseContainerType);
        map.AddContainer(container);

        while (currLink != null)
        {
            Tile tile = new Tile(currLink.currTile.x, currLink.currTile.y, map, pathTileType);
            map.SetTile(currLink.currTile.x, currLink.currTile.y, tile, container);

            currLink = currLink.parentLink;
        }

    }


    private Link AddToQueue(
        Tile currTile,
        Link parentLink,
        Queue<Link> queue,
        Dictionary<Tile, bool> tileHasBeenSearched
    )
    {
        if (!IsValidTile(currTile, tileHasBeenSearched)) return null;

        Link currLink = new Link(currTile, parentLink);
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

        return AddToQueue(currTile, parentLink, queue, tileHasBeenSearched);
    }

    // returns true if the door has been reached, otherwise false
    private bool BFSStep(List<Pair<int, int>> path, Dictionary<Tile, bool> tileHasBeenSearched, Door door)
    {
        //A door has been found
        if(Utilities.EndOfList(path).First == door.x && Utilities.EndOfList(path).Second == door.y)
        {
            return true;
        }

        
        int currX = Utilities.EndOfList(path).First;
        int currY = Utilities.EndOfList(path).Second;
        int destX = door.x;
        int destY = door.y;

        float angle = Utilities.GetAngle(currX, currY, destX, destY);

        if(Utilities.RandomIntInRange(0, 700) == 0)
        {
            Debug.Log(currX + " " + currY + " " + destX + " " + destY + " " + angle);
        }

        Utilities.Direction[] directionAttemptOrder;

        if (angle >= 0 && angle < 45)
            directionAttemptOrder = new Utilities.Direction[] { Utilities.Direction.EAST, Utilities.Direction.NORTH, Utilities.Direction.SOUTH, Utilities.Direction.WEST };
        else if (angle >= 45 && angle < 90)
            directionAttemptOrder = new Utilities.Direction[] { Utilities.Direction.NORTH, Utilities.Direction.EAST, Utilities.Direction.SOUTH, Utilities.Direction.WEST };
        else if (angle >= 90 && angle < 135)
            directionAttemptOrder = new Utilities.Direction[] { Utilities.Direction.NORTH, Utilities.Direction.WEST, Utilities.Direction.EAST, Utilities.Direction.SOUTH };
        else if (angle >= 135 && angle < 180)
            directionAttemptOrder = new Utilities.Direction[] { Utilities.Direction.WEST, Utilities.Direction.NORTH, Utilities.Direction.SOUTH, Utilities.Direction.EAST };
        else if (angle >= 180 && angle < 225)
            directionAttemptOrder = new Utilities.Direction[] { Utilities.Direction.WEST, Utilities.Direction.SOUTH, Utilities.Direction.NORTH, Utilities.Direction.EAST };
        else if (angle >= 225 && angle < 270)
            directionAttemptOrder = new Utilities.Direction[] { Utilities.Direction.SOUTH, Utilities.Direction.WEST, Utilities.Direction.SOUTH, Utilities.Direction.NORTH };
        else if (angle >= 270 && angle < 315)
            directionAttemptOrder = new Utilities.Direction[] { Utilities.Direction.SOUTH, Utilities.Direction.EAST, Utilities.Direction.NORTH, Utilities.Direction.WEST };
        else // if (angle >= 315 && angle < 360)
            directionAttemptOrder = new Utilities.Direction[] { Utilities.Direction.EAST, Utilities.Direction.SOUTH, Utilities.Direction.NORTH, Utilities.Direction.WEST };
        

        // for each direction at the end of the path
        foreach (Utilities.Direction direction in directionAttemptOrder)// Utilities.Direction.GetValues(typeof(Utilities.Direction)))
        {
            if(AddToPath(path, direction, tileHasBeenSearched))
            {
                if(BFSStep(path, tileHasBeenSearched, door))
                {
                    // recursing further reached the door, thus no
                    // more searching needs to be done
                    return true;
                }

                //that BFSStep did not reach a door, undo it
                Utilities.RemoveEndOfList(path);
            }
        }

        return false;
    }


    private void ModifyMapUsingPath(List<Pair<int, int>> path)
    {
        // todo: choose a more specific container type for the path's container
        Container container = new Container(map, baseContainerType);
        map.AddContainer(container);

        foreach(Pair<int, int> cord in path)
        {
            Tile tile = new Tile(cord.First, cord.Second, map, pathTileType);
            map.SetTile(cord.First, cord.Second, tile, container);
        }

    }
    
    // If can add to path, do so and return true. If cant (tile has been searched already),
    // dont add to path and return false.
    private bool AddToPath(List<Pair<int, int>> path, Utilities.Direction direction, Dictionary<Tile, bool> tileHasBeenSearched)
    {
        int currX = Utilities.EndOfList(path).First;
        int currY = Utilities.EndOfList(path).Second;

        int newX, newY;

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
            default:
                newX = currX;
                newY = currY;
                break;
        }

        Tile potentialTile = map.GetTile(newX, newY);

        if (!IsValidTile(potentialTile, tileHasBeenSearched)) return false;

        path.Add(new Pair<int, int>(newX, newY));

        SearchTile(tileHasBeenSearched, path);

        return true;
    }

    private bool IsValidTile(Tile potentialTile, Dictionary<Tile, bool> tileHasBeenSearched)
    {
        if(
            potentialTile == null ||
            potentialTile.tileType != earthTileType ||
            TileHasBeenSearched(tileHasBeenSearched, potentialTile)
        )
        {
            return false;
        }

        return true;
    }

    private bool TileHasBeenSearched(Dictionary<Tile, bool> tileHasBeenSearched, Tile tile)
    {
        // if tileHasBeenSearched has a key and returns true, it means the tile
        // has been searched. otherwise, the tile has not been searched.
        return (tileHasBeenSearched.ContainsKey(tile) && tileHasBeenSearched[tile]);
    }

    private bool HasTileBeenSearched(Dictionary<Tile, bool> tileHasBeenSearched, int x, int y)
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
        room.GenRoom();
    }
    


}

public class Link
{
    public Tile currTile;
    public Link parentLink;

    public Link(Tile currTile, Link parentLink)
    {
        this.currTile = currTile;
        this.parentLink = parentLink;
    }
}