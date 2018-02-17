using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class MonsterMoveAction : EntityAction 
{
	public float speed = 1f;

	public override bool Validate(GameObject obj)
	{
		return true;
	}
}
