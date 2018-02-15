using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class EntityActionManager : MonoBehaviour {

	//public Entity entity;
	public Action action;

	// Use this for initialization
	void Start () {
		
	}
	
	// Update is called once per frame
	void Update () 
	{
		if (this.gameObject == null) 
		{
			return;
		}

		if (action.validate (this.gameObject)) 
		{
			
			action.execute (this.gameObject);
		}
	}
}
