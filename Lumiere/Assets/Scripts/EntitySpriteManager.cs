using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EntitySpriteManager : MonoBehaviour {

	public Entity entity;	//Entity to render onto the script's GameObject

	/// <summary>
	/// On initialization, set sprite renderer's sprite to be the entity's sprite.
	/// </summary>
	void Start () {
		setSprite();
	}

	/// <summary>
	/// Sets the sprite.
	/// </summary>
	public void setSprite() {
		SpriteRenderer renderer = GetComponent<SpriteRenderer>();
		renderer.sprite = entity.getSprite();
	}

}
