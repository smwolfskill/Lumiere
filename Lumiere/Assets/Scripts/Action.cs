using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class Action : ScriptableObject 
{
	public abstract bool validate (GameObject obj);
	public abstract bool execute(GameObject obj);

}
