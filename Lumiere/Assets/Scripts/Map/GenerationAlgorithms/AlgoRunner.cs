using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AlgoRunner : MonoBehaviour {

    public GenAlgo genAlgo;

    public enum GenerationAlgorithm
    {
        SimpleGenAlgo
    };
    public GenerationAlgorithm generationAlgorithm;

    [Header("GenAlgo Simple Properties")]
    public int roomAttempts = 30;
    public int pathAttempts = 30;
    public int pathDirectionChangeLikelihood = 10; // Larger means less likely to change directions.

    [Header("Map Properties")]
    public int width = 50;
    public int height = 50;
    public int tileOffset = 5;

    private Map map;

    void Start ()
    {

        map = new Map(new Vector2Int(width, height), tileOffset);

        switch (generationAlgorithm)
        {
            case GenerationAlgorithm.SimpleGenAlgo:
                genAlgo = new SimpleGenAlgo(
                    map,
                    roomAttempts,
                    pathAttempts,
                    pathDirectionChangeLikelihood
                );
                break;
        }


        genAlgo.GenerateMap(gameObject);
	}

}
