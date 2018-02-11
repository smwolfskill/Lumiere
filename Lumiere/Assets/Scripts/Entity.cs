using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using System.Security.Cryptography;

public abstract class Entity : BaseObject {
	//protected EntityAction[] actions;

	abstract public bool spawn();
}