using System;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Pathfinding
{
    private Map map;

    public Pathfinding(Map map)
    {
        this.map = map;
    }

    public List<TileObj> GetPath(Vector2 start, Vector2 end)
    {
        TileObj startTile = GetNearestTile(start);
        TileObj endTile = GetNearestTile(end);

        List<TileObj> closedSet = new List<TileObj>();
        List<TileObj> openSet = new List<TileObj>();
        openSet.Add(startTile);
        Dictionary<TileObj, TileObj> cameFrom = new Dictionary<TileObj, TileObj>();
        Dictionary<TileObj, int> gScores = new Dictionary<TileObj, int> ();
        Dictionary<TileObj, int> fScores = new Dictionary<TileObj, int>();

        InitializeScores(gScores);
        gScores[startTile] = 0;
        InitializeScores(fScores);
        fScores[startTile] = GetHeuristicCost(startTile, endTile);

        while(openSet.Count > 0)
        {
            TileObj current = GetLowestScoreTile(openSet, fScores);
            if(current == endTile)
            {
                //reconstruct path
                return ReconstructPath(cameFrom, current);
            }

            openSet.Remove(current);
            closedSet.Add(current);
            foreach(TileObj neighbor in GetNeighbors(current))
            {
                if(!closedSet.Contains(neighbor))
                {
                    if(!openSet.Contains(neighbor))
                    {
                        openSet.Add(neighbor);
                    }

                    int tentativeGScore = gScores[current] + GetHeuristicCost(current, neighbor);
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

    private List<TileObj> ReconstructPath(Dictionary<TileObj, TileObj> cameFrom, TileObj current)
    {
        List<TileObj> totalPath = new List<TileObj>();
        totalPath.Add(current);
        while(cameFrom.ContainsKey(current))
        {
            current = cameFrom[current];
            totalPath.Add(current);
        }

        return totalPath;
    }

    private TileObj GetLowestScoreTile(List<TileObj> openSet, Dictionary<TileObj, int> scores)
    {
        TileObj lowestTileObj = openSet[0];
        foreach(TileObj tile in openSet)
        {
            if(scores[lowestTileObj] < scores[tile])
            {
                lowestTileObj = tile;
            }
        }

        return lowestTileObj;
    }

    private TileObj GetNearestTile(Vector2 location)
    {
        return map.GetTile(Mathf.RoundToInt(location.x/map.tileOffset), Mathf.RoundToInt(location.y/map.tileOffset));
    }

    private int GetHeuristicCost(TileObj startTile, TileObj endTile)
    {
        Vector2 start = new Vector2(startTile.x * map.tileOffset, startTile.y * map.tileOffset);
        Vector2 end = new Vector2(endTile.x * map.tileOffset, endTile.y * map.tileOffset);
        return (int) Vector2.Distance(start, end);
    }

    private List<TileObj> GetNeighbors(TileObj current)
    {
        List<TileObj> neighbors = new List<TileObj>();
        foreach(Utilities.Direction direction in Enum.GetValues(typeof(Utilities.Direction)))
        {
            neighbors.Add(current.GetNeighbor(direction));
        }

        return neighbors;
    }

    private void InitializeScores(Dictionary<TileObj, int> scores)
    {
        List<TileObj> tiles = map.GetTiles();
        foreach(TileObj tile in tiles)
        {
            scores[tile] = int.MaxValue;
        }
    }
}
