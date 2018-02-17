using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class MonsterMoveAction : EntityAction 
{
	public float speed = 1f;

	public override bool Validate(GameObject obj)
	{
        //TODO: Replace this in future iterations when AI is implemented
		return true;
	}
}
