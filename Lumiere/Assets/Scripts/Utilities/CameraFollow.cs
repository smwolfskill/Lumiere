using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class CameraFollow : MonoBehaviour
{
    private Transform target;
    Vector3 originalPosition;
    // Use this for initialization
    Vector3 offset;

    void Start ()
    {
    }

    // Update is called once per frame
    void Update ()
    {
        if (target != null) {
            transform.position = target.position + offset;
        }
	}

    public void findPlayerTransform()
    {
        target = GameObject.Find ("Player").transform;
        //Set the offset so that the camera can see the player and the map
        offset = new Vector3(0, 0, -10);
    }

}
