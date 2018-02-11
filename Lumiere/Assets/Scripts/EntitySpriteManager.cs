using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EntitySpriteManager : MonoBehaviour {

	public Entity entity;

	/// <summary>
	/// On initialization, set sprite renderer's sprite to be the entity's sprite.
	/// </summary>
	void Start () {
		SpriteRenderer renderer = GetComponent<SpriteRenderer>();
		renderer.sprite = entity.getSprite();
	}
	
	// Update is called once per frame
	/*void Update () {
		
	}*/
}
