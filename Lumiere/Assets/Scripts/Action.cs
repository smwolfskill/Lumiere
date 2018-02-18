using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public abstract class Action : ScriptableObject {

	/// <summary>
	/// Execute this action.
	/// </summary>
	/// <returns> true if action executed successfully. </returns>
	abstract public bool execute();

}
