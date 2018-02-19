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
        target = GameObject.FindGameObjectWithTag("Player").transform;
        offset = transform.position - target.position;
    }

    // Update is called once per frame
    void Update ()
    {
        transform.position = target.position + offset;
	}
}
