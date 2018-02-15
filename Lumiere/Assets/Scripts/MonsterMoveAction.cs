using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class MonsterMoveAction : Action 
{
	public float speed;

	public override bool Validate (GameObject obj)
	{
		return true;
	}
}
