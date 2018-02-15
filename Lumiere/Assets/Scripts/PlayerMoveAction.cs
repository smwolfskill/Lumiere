using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[CreateAssetMenu (menuName = "Lumiere/Actions/PlayerMoveAction")]
public class PlayerMoveAction : Action 
{
	public float speed = 1f;

	public override bool Validate (GameObject obj)
	{
		return true;
	}

	public override bool Execute (GameObject obj)
	{
		Rigidbody2D rigidbody = obj.GetComponent<Rigidbody2D> ();
		if (rigidbody == null) 
		{
			return false;
		}

		float h = Input.GetAxis ("Horizontal");
		float v = Input.GetAxis ("Vertical");

		if (Mathf.Abs(h) >= Mathf.Abs(v)) 
		{
			rigidbody.velocity = new Vector2 (h, 0f);
		} 
		else 
		{
			rigidbody.velocity = new Vector2 (0f, v);	
		}

		return true;
	}
}
