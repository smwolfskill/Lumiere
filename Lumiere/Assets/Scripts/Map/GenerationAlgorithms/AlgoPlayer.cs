using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class AlgoPlayer : MonoBehaviour {

    public GenAlgo genAlgo;

	void Start ()
    {
        genAlgo = new SimpleGenAlgo();

        genAlgo.GenerateMap(gameObject);
	}

}
