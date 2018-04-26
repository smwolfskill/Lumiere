using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class NextLevel : MonoBehaviour {

    public MapManager mapManager;

    private void OnCollisionEnter2D(Collision2D collision)
    {
        if(collision.gameObject.tag == "Player")
        {
            mapManager.DestroyMap();
            mapManager.GenMap();
        }
    }
}
