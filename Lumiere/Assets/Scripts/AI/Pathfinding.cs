using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

/// <summary>
/// A pathfinding utility class that uses the map and a start and end position to calculate valid paths for AI.
/// </summary>
public class Pathfinding
{
    /// <summary>
    /// The map to create the Pathfinding object with. This is the map that is generated in game.
    /// </summary>
    private Map map;

    /// <summary>
    /// Initializes a new instance of the <see cref="Pathfinding"/> class.
    /// </summary>
    /// <param name="map">The generated game map.</param>
    public Pathfinding(Map map)
    {
        this.map = map;
    }

    /// <summary>
    /// Gets a valid tile path from the start location to the end location.
    /// </summary>
    /// <returns>A list of tiles representing the path to traverse.</returns>
    /// <param name="start">The starting location to create a path from.</param>
    /// <param name="end">The end or target location to path towards.</param>
    public List<Tile> GetPath(Vector2 start, Vector2 end)
    {
        Tile startTile = GetNearestTile(start);
        Tile endTile = GetNearestTile(end);

        List<Tile> closedSet = new List<Tile>();
        List<Tile> openSet = new List<Tile>();
        openSet.Add(startTile);
        Dictionary<Tile, Tile> cameFrom = new Dictionary<Tile, Tile>();
        Dictionary<Tile, int> gScores = new Dictionary<Tile, int> ();
        Dictionary<Tile, int> fScores = new Dictionary<Tile, int>();

        InitializeScores(gScores);
        gScores[startTile] = 0;
        InitializeScores(fScores);
        fScores[startTile] = GetHeuristicCost(startTile, endTile);

        while(openSet.Count > 0)
        {
            Tile current = GetLowestScoreTile(openSet, fScores);
            if(current == endTile)
            {
                //reconstruct path
                return ReconstructPath(cameFrom, current);
            }

            openSet.Remove(current);
            closedSet.Add(current);
            foreach(Tile neighbor in GetNeighbors(current))
            {
                if(!closedSet.Contains(neighbor))
                {
                    if(!openSet.Contains(neighbor))
                    {
                        openSet.Add(neighbor);
                    }

                    int tentativeGScore = gScores[current] + GetDistance(current, neighbor);
                    if(tentativeGScore < gScores[neighbor])
                    {
                        cameFrom[neighbor] = current;
                        gScores[neighbor] = tentativeGScore;
                        fScores[neighbor] = gScores[neighbor] + GetHeuristicCost(neighbor, endTile);
                    }

                }

            }
        }

        return null;
    }

    /// <summary>
    /// Reconstructs the path backwards from a map that maps tiles to their previous tiles, with the path ending at the current tile.
    /// </summary>
    /// <returns>The path to be constructed.</returns>
    /// <param name="cameFrom">The dictionary that represents tiles and their parents (the tiles they came from).</param>
    /// <param name="current">The tile to reconstruct a path towards.</param>
    private List<Tile> ReconstructPath(Dictionary<Tile, Tile> cameFrom, Tile current)
    {
        List<Tile> totalPath = new List<Tile>();
        totalPath.Add(current);
        while(cameFrom.ContainsKey(current))
        {
            current = cameFrom[current];
            totalPath.Add(current);
        }

        return totalPath;
    }

    /// <summary>
    /// Gets the tile in openSet with the lowest score.
    /// </summary>
    /// <returns>The tile in openSet with the lowest score.</returns>
    /// <param name="openSet">The list of tiles yet to be explored.</param>
    /// <param name="scores">A dictionary that maps a tile to its score.</param>
    private Tile GetLowestScoreTile(List<Tile> openSet, Dictionary<Tile, int> scores)
    {
        Tile lowestTile = openSet[0];
        foreach(Tile tile in openSet)
        {
            if(scores[lowestTile] < scores[tile])
            {
                lowestTile = tile;
            }
        }

        return lowestTile;
    }

    /// <summary>
    /// Gets the nearest tile to the given location.
    /// </summary>
    /// <returns>The nearest tile to the given location.</returns>
    /// <param name="location">Location in world-space.</param>
    private Tile GetNearestTile(Vector2 location)
    {
        return map.GetTile((int)(location.x/map.tileOffset), (int)(location.y/map.tileOffset));
    }

    /// <summary>
    /// Gets the heuristic cost of travelling from startTile to endTile. We use the Euclidean distance as our heuristic here.
    /// </summary>
    /// <returns>The heuristic cost of travelling from startTile to endTile.</returns>
    /// <param name="startTile">Start tile.</param>
    /// <param name="endTile">End tile.</param>
    private int GetHeuristicCost(Tile startTile, Tile endTile)
    {
        Vector2 start = new Vector2(startTile.x * map.tileOffset, startTile.y * map.tileOffset);
        Vector2 end = new Vector2(endTile.x * map.tileOffset, endTile.y * map.tileOffset);
        return (int) Vector2.Distance(start, end);
    }

    /// <summary>
    /// Estimates the travel distance of travelling from startTile to endTile. We emphasize that unwalkable tiles are unreachable, and therefore have a distance of infinity.
    /// </summary>
    /// <returns>The distance of travelling from startTile to endTile.</returns>
    /// <param name="startTile">Start tile.</param>
    /// <param name="endTile">End tile.</param>
    private int GetDistance(Tile startTile, Tile endTile)
    {
        if (!endTile.IsWalkable ()) 
        {
            return int.MaxValue;
        }
        return GetHeuristicCost(startTile, endTile);
    }

    /// <summary>
    /// Gets all the valid neighbors of the tile specified and enumerates them in a list.
    /// </summary>
    /// <returns>The list of neighbors of the tile specified.</returns>
    /// <param name="current">The specified tile.</param>
    private List<Tile> GetNeighbors(Tile current)
    {
        List<Tile> neighbors = new List<Tile>();
        foreach(Utilities.Direction direction in Enum.GetValues(typeof(Utilities.Direction)))
        {
            Tile neighbor = current.GetNeighbor (direction);
            if (neighbor != null) 
            {
                neighbors.Add(neighbor);
            }

        }

        return neighbors;
    }

    /// <summary>
    /// Initializes the scores dictionary and maps all tiles to an initial score of infinity. These will be changed when a lower score is found from the search algorithm.
    /// </summary>
    /// <param name="scores">A dictionary that maps each tile to its score.</param>
    private void InitializeScores(Dictionary<Tile, int> scores)
    {
        List<Tile> tiles = map.GetTiles();
        foreach(Tile tile in tiles)
        {
            scores[tile] = int.MaxValue;
        }
    }
}
