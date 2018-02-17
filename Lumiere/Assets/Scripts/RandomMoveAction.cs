using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu(menuName = "Lumiere/Actions/RandomMoveAction")]
public class RandomMoveAction : MonsterMoveAction 
{
	public float directionChangeTimer = 5f;
	private float timer = 0f;
	private bool initialized = false;

	public override bool Validate(GameObject obj)
	{
        if(!base.Validate(obj)) { return false; }

		if (!initialized) 
		{
            initialized = true;
            return true;
		}
		
		if (timer >= directionChangeTimer) 
		{
			timer = Time.deltaTime;
			return true;
		}

		timer += Time.deltaTime;
		return false;
	}

	public override bool Execute(GameObject obj)
	{
		Rigidbody2D rigidbody = obj.GetComponent<Rigidbody2D> ();
		if (rigidbody == null) 
		{
			return false;
		}

		float isHorizontal = Random.Range (-1.0f, 1.0f);
		float directionModifier = Mathf.Sign(Random.Range (-1.0f, 1.0f));
		if (isHorizontal >= 0.0f) 
		{
			rigidbody.velocity = new Vector2 (directionModifier * speed, 0f);
		} 
		else 
		{
			rigidbody.velocity = new Vector2 (0f, directionModifier * speed);	
		}

		return true;
	}
}
