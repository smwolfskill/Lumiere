using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EntitySpriteManager : MonoBehaviour {

	public Entity entity;

	void Start () {
		//Set our game object's sprite renderer's sprite to be the entity's sprite
		SpriteRenderer renderer = GetComponent<SpriteRenderer>();
		renderer.sprite = entity.getSprite();
	}
	
	// Update is called once per frame
	/*void Update () {
		
	}*/
}
