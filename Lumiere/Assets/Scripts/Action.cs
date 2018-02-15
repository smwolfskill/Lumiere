using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class Action : ScriptableObject 
{
	public abstract bool Validate (GameObject obj);
	public abstract bool Execute(GameObject obj);

}
